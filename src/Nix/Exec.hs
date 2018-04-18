{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Nix.Exec where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.Fix
import           Control.Monad.IO.Class
import           Control.Monad.Reader (MonadReader, asks)
import           Control.Monad.Trans.Reader hiding (asks)
import qualified Data.ByteString as BS
import           Data.Coerce
import           Data.Fix
import           Data.Functor.Compose
import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as M
import           Data.IORef
import           Data.List
import           Data.List.Split
import           Data.Maybe (mapMaybe)
import           Data.Text (Text)
import qualified Data.Text as Text
import           Nix.Atoms
import           Nix.Context
import           Nix.Convert
import           Nix.Effects
import           Nix.Eval
import qualified Nix.Eval as Eval
import           Nix.Expr
import           Nix.Normal
import           Nix.Options
import           Nix.Parser
import           Nix.Pretty
import           Nix.Scope
import           Nix.Stack
import           Nix.Thunk
import           Nix.Utils
import           Nix.Value
import           System.Directory
import           System.Environment
import           System.Exit (ExitCode (ExitSuccess))
import           System.FilePath
import qualified System.Info
import           System.Posix.Files
import           System.Process (readProcessWithExitCode)
import {-# SOURCE #-} Nix.Entry as Entry

nverr :: forall e m a. MonadNix e m => String -> m a
nverr = evalError @(NValue m)

instance MonadNix e m => MonadThunk (NValue m) (NThunk m) m where
    thunk = fmap coerce . buildThunk
    force = forceThunk . coerce
    value = coerce . valueRef

instance MonadNix e m => MonadEval (NValue m) m where
    freeVariable var =
        nverr $ "Undefined variable '" ++ Text.unpack var ++ "'"

    evalCurPos = do
        Fix (Compose (Ann (SrcSpan delta _) _)) : _ <-
            asks (mapMaybe (either (const Nothing) Just)
                 . view @_ @Frames hasLens)
        toValue delta

    evalConstant    = pure . NVConstant
    evalString      = (pure .) . NVStr
    evalLiteralPath = fmap NVPath . makeAbsolutePath
    evalEnvPath     = fmap NVPath . findEnvPath
    evalUnary       = execUnaryOp
    evalBinary      = execBinaryOp
    evalWith        = evalWithAttrSet

    evalIf c t f = fromValue c >>= \b -> if b then t else f

    evalAssert c body = fromValue c >>= \b ->
        if b then body else nverr "assertion failed"

    evalApp = callFunc
    evalAbs = (pure .) . NVClosure

    evalError = throwError

infixl 1 `callFunc`
callFunc :: MonadNix e m => NValue m -> m (NValue m) -> m (NValue m)
callFunc fun arg = case fun of
    NVClosure params f -> do
        traceM $ "callFunc:NVFunction taking " ++ show params
        f arg
    NVBuiltin name f -> do
        traceM $ "callFunc:NVBuiltin " ++ name
        f arg
    s@(NVSet m _) | Just f <- M.lookup "__functor" m -> do
        traceM "callFunc:__functor"
        force f $ (`callFunc` pure s) >=> (`callFunc` arg)
    x -> arg >>= \arg' ->
        throwError $ "Attempt to call non-function '" ++ show x
            ++ "' with arg: " ++ show arg'

execUnaryOp
    :: (Framed e m, MonadVar m, MonadFile m)
    => NUnaryOp -> NValue m -> m (NValue m)
execUnaryOp op arg = do
    traceM "NUnary"
    case arg of
        NVConstant c -> case (op, c) of
            (NNeg, NInt   i) -> return $ NVConstant $ NInt   (-i)
            (NNeg, NFloat f) -> return $ NVConstant $ NFloat (-f)
            (NNot, NBool  b) -> return $ NVConstant $ NBool  (not b)
            _ -> throwError $ "unsupported argument type for unary operator "
                     ++ show op
        x -> throwError $ "argument to unary operator"
                ++ " must evaluate to an atomic type: " ++ show x

execBinaryOp
    :: forall e m. (MonadNix e m, MonadEval (NValue m) m)
    => NBinaryOp -> NValue m -> m (NValue m) -> m (NValue m)

execBinaryOp NOr larg rarg = fromNix larg >>= \l ->
    if l
    then toNix True
    else rarg >>= fromNix @Bool >>= toNix

execBinaryOp NAnd larg rarg = fromNix larg >>= \l ->
    if l
    then rarg >>= fromNix @Bool >>= toNix
    else toNix False

-- jww (2018-04-08): Refactor so that eval (NBinary ..) *always* dispatches
-- based on operator first
execBinaryOp op larg rarg = do
    let lval = larg
    rval <- traceM "NBinary:right" >> rarg

    let unsupportedTypes =
            "Unsupported argument types for binary operator "
                ++ show op ++ ": " ++ show lval ++ ", " ++ show rval
        numBinOp :: (forall a. Num a => a -> a -> a) -> NAtom -> NAtom
                 -> m (NValue m)
        numBinOp f = numBinOp' f f
        numBinOp'
            :: (Integer -> Integer -> Integer)
            -> (Float -> Float -> Float)
            -> NAtom -> NAtom -> m (NValue m)
        numBinOp' intF floatF l r = case (l, r) of
            (NInt   li, NInt   ri) ->
                toValue $             li `intF`               ri
            (NInt   li, NFloat rf) ->
                toValue $ fromInteger li `floatF`             rf
            (NFloat lf, NInt   ri) ->
                toValue $             lf `floatF` fromInteger ri
            (NFloat lf, NFloat rf) ->
                toValue $             lf `floatF`             rf
            _ -> nverr unsupportedTypes

        nverr = evalError @(NValue m)

    case (lval, rval) of
        (NVConstant lc, NVConstant rc) -> case (op, lc, rc) of
            (NEq,  _, _)   -> toValue =<< valueEq lval rval
            (NNEq, _, _)   -> toValue . not =<< valueEq lval rval
            (NLt,  l, r)   -> toValue $ l <  r
            (NLte, l, r)   -> toValue $ l <= r
            (NGt,  l, r)   -> toValue $ l >  r
            (NGte, l, r)   -> toValue $ l >= r
            (NAnd,  _, _)  -> nverr "should be impossible: && is handled above"
            (NOr,   _, _)  -> nverr "should be impossible: || is handled above"
            (NPlus,  l, r) -> numBinOp (+) l r
            (NMinus, l, r) -> numBinOp (-) l r
            (NMult,  l, r) -> numBinOp (*) l r
            (NDiv,   l, r) -> numBinOp' div (/) l r
            (NImpl, NBool l, NBool r) -> toValue $ not l || r
            _ -> nverr unsupportedTypes

        (NVStr ls lc, NVStr rs rc) -> case op of
            NPlus -> pure $ NVStr (ls `mappend` rs) (lc `mappend` rc)
            NEq   -> toValue =<< valueEq lval rval
            NNEq  -> toValue . not =<< valueEq lval rval
            NLt   -> toValue $ ls <  rs
            NLte  -> toValue $ ls <= rs
            NGt   -> toValue $ ls >  rs
            NGte  -> toValue $ ls >= rs
            _ -> nverr unsupportedTypes

        (NVStr _ _, NVConstant NNull) -> case op of
            NEq   -> toValue =<< valueEq lval (NVStr "" mempty)
            NNEq  -> toValue . not =<< valueEq lval (NVStr "" mempty)
            _ -> nverr unsupportedTypes

        (NVConstant NNull, NVStr _ _) -> case op of
            NEq   -> toValue =<< valueEq (NVStr "" mempty) rval
            NNEq  -> toValue . not =<< valueEq (NVStr "" mempty) rval
            _ -> nverr unsupportedTypes

        (NVSet ls lp, NVSet rs rp) -> case op of
            NUpdate -> pure $ NVSet (rs `M.union` ls) (rp `M.union` lp)
            NEq     -> toValue =<< valueEq lval rval
            NNEq    -> toValue . not =<< valueEq lval rval
            _ -> nverr unsupportedTypes

        (NVList ls, NVList rs) -> case op of
            NConcat -> pure $ NVList $ ls ++ rs
            NEq     -> toValue =<< valueEq lval rval
            NNEq    -> toValue . not =<< valueEq lval rval
            _ -> nverr unsupportedTypes

        (NVList ls, NVConstant NNull) -> case op of
            NConcat -> pure $ NVList ls
            NEq     -> toValue =<< valueEq lval (NVList [])
            NNEq    -> toValue . not =<< valueEq lval (NVList [])
            _ -> nverr unsupportedTypes

        (NVConstant NNull, NVList rs) -> case op of
            NConcat -> pure $ NVList rs
            NEq     -> toValue =<< valueEq (NVList []) rval
            NNEq    -> toValue . not =<< valueEq (NVList []) rval
            _ -> nverr unsupportedTypes

        (NVPath p, NVStr s _) -> case op of
            -- jww (2018-04-13): Do we need to make the path absolute here?
            NEq   -> toValue $ p == Text.unpack s
            NNEq  -> toValue $ p /= Text.unpack s
            NPlus -> NVPath <$> makeAbsolutePath (p `mappend` Text.unpack s)
            _ -> nverr unsupportedTypes

        (NVPath ls, NVPath rs) -> case op of
            NPlus -> NVPath <$> makeAbsolutePath (ls ++ rs)
            _ -> nverr unsupportedTypes

        _ -> nverr unsupportedTypes

newtype Lazy m a = Lazy
    { runLazy :: ReaderT (Context (Lazy m) (NThunk (Lazy m))) m a }
    deriving (Functor, Applicative, Alternative, Monad, MonadPlus,
              MonadFix, MonadIO,
              MonadReader (Context (Lazy m) (NThunk (Lazy m))))

instance MonadIO m => MonadVar (Lazy m) where
    type Var (Lazy m) = IORef

    newVar = liftIO . newIORef
    readVar = liftIO . readIORef
    writeVar = (liftIO .) . writeIORef
    atomicModifyVar = (liftIO .) . atomicModifyIORef

instance MonadIO m => MonadFile (Lazy m) where
    readFile = liftIO . BS.readFile

instance MonadCatch m => MonadCatch (Lazy m) where
    catch (Lazy (ReaderT m)) f = Lazy $ ReaderT $ \e ->
        catch (m e) ((`runReaderT` e) . runLazy . f)

instance MonadThrow m => MonadThrow (Lazy m) where
    throwM = Lazy . throwM

instance (MonadFix m, MonadCatch m, MonadThrow m, MonadIO m)
      => MonadEffects (Lazy m) where
    addPath path = do
        (exitCode, out, _) <-
            liftIO $ readProcessWithExitCode "nix-store" ["--add", path] ""
        case exitCode of
          ExitSuccess -> do
            let dropTrailingLinefeed p = take (length p - 1) p
            return $ StorePath $ dropTrailingLinefeed out
          _ -> throwError $ "addPath: failed: nix-store --add " ++ show path

    makeAbsolutePath origPath = do
        absPath <- if isAbsolute origPath then pure origPath else do
            cwd <- do
                mres <- lookupVar @_ @(NThunk (Lazy m)) "__cur_file"
                case mres of
                    Nothing -> liftIO getCurrentDirectory
                    Just v -> force v $ \case
                        NVPath s -> return $ takeDirectory s
                        v -> throwError $ "when resolving relative path,"
                                ++ " __cur_file is in scope,"
                                ++ " but is not a path; it is: "
                                ++ show v
            pure $ cwd <///> origPath
        liftIO $ removeDotDotIndirections <$> canonicalizePath absPath

    findEnvPath = findEnvPathM

    pathExists = liftIO . fileExist

    -- jww (2018-03-29): Cache which files have been read in.
    importPath scope origPath = do
        path <- liftIO $ pathToDefaultNixFile origPath
        mres <- lookupVar @(Context (Lazy m) (NThunk (Lazy m)))
                         "__cur_file"
        path' <- case mres of
            Nothing  -> do
                traceM "No known current directory"
                return path
            Just p -> force p $ normalForm >=> \case
                Fix (NVPath p') -> do
                    traceM $ "Current file being evaluated is: "
                        ++ show p'
                    return $ takeDirectory p' </> path
                x -> error $ "How can the current directory be: " ++ show x

        traceM $ "Importing file " ++ path'

        withStringContext ("While importing file " ++ show path') $ do
            eres <- Lazy $ parseNixFileLoc path'
            case eres of
                Failure err  -> error $ "Parse failed: " ++ show err
                Success expr -> do
                    let ref = value @_ @_ @(Lazy m) (NVPath path')
                    -- Use this cookie so that when we evaluate the next
                    -- import, we'll remember which directory its containing
                    -- file was in.
                    pushScope (M.singleton "__cur_file" ref)
                        (pushScope scope (framedEvalExpr Eval.eval expr))

    getEnvVar = liftIO . lookupEnv

    getCurrentSystemOS = return $ Text.pack System.Info.os

    -- Invert the conversion done by GHC_CONVERT_CPU in GHC's aclocal.m4
    getCurrentSystemArch = return $ Text.pack $ case System.Info.arch of
      "i386" -> "i686"
      arch -> arch

    listDirectory         = liftIO . System.Directory.listDirectory
    getSymbolicLinkStatus = liftIO . System.Posix.Files.getSymbolicLinkStatus

    derivationStrict v = do
        v' <- normalForm v
        nixInstantiateExpr $ "derivationStrict " ++ show (prettyNixValue v')

    nixInstantiateExpr expr = do
        (exitCode, out, _) <-
            liftIO $ readProcessWithExitCode "nix-instantiate"
                [ "--eval", "--expr", expr] ""
        case exitCode of
            ExitSuccess ->
                case parseNixTextLoc (Text.pack out) of
                    Failure err ->
                        throwError $ "Error parsing output of nix-instantiate: "
                            ++ show err
                    Success v -> framedEvalExpr Eval.eval v
            err -> throwError $ "nix-instantiate failed: " ++ show err

runLazyM :: Options -> MonadIO m => Lazy m a -> m a
runLazyM opts = flip runReaderT (newContext opts) . runLazy

-- | Incorrectly normalize paths by rewriting patterns like @a/b/..@ to @a@.
--   This is incorrect on POSIX systems, because if @b@ is a symlink, its
--   parent may be a different directory from @a@. See the discussion at
--   https://hackage.haskell.org/package/directory-1.3.1.5/docs/System-Directory.html#v:canonicalizePath
removeDotDotIndirections :: FilePath -> FilePath
removeDotDotIndirections = intercalate "/" . go [] . splitOn "/"
    where go s [] = reverse s
          go (_:s) ("..":rest) = go s rest
          go s (this:rest) = go (this:s) rest

-- Given a path, determine the nix file to load
pathToDefaultNixFile :: FilePath -> IO FilePath
pathToDefaultNixFile p = do
    isDir <- doesDirectoryExist p
    pure $ if isDir then p </> "default.nix" else p

infixr 9 <///>
(<///>) :: FilePath -> FilePath -> FilePath
x <///> y | isAbsolute y || "." `isPrefixOf` y = x </> y
          | otherwise = joinByLargestOverlap x y
  where
    joinByLargestOverlap (splitDirectories -> xs) (splitDirectories -> ys) =
        joinPath $ head [ xs ++ drop (length tx) ys
                        | tx <- tails xs, tx `elem` inits ys ]

nixFilePath :: (MonadEffects m, MonadIO m) => FilePath -> m (Maybe FilePath)
nixFilePath path = do
    path <- makeAbsolutePath path
    exists <- liftIO $ doesDirectoryExist path
    path' <- if exists
            then makeAbsolutePath $ path </> "default.nix"
            else return path
    exists <- liftIO $ doesFileExist path'
    return $ if exists then Just path' else Nothing

findEnvPathM :: forall e m. (MonadNix e m, MonadIO m)
             => FilePath -> m FilePath
findEnvPathM name = do
    mres <- lookupVar @_ @(NThunk m) "__nixPath"
    mpath <- case mres of
        Nothing -> error "impossible"
        Just x -> force x $ fromValue >=> \(l :: [NThunk m]) ->
            foldM go Nothing l
    case mpath of
        Nothing ->
            throwError $ "file '" ++ name
                ++ "' was not found in the Nix search path"
                ++ " (add it using $NIX_PATH or -I)"
        Just path -> return path
  where
    go :: Maybe FilePath -> NThunk m -> m (Maybe FilePath)
    go p@(Just _) _ = pure p
    go Nothing l = force l $ fromValue >=> \(s :: HashMap Text (NThunk m)) ->
        case M.lookup "path" s of
            Just p -> force p $ fromValue >=> \(Path path) ->
                case M.lookup "prefix" s of
                    Nothing -> tryPath path Nothing
                    Just pf -> force pf $ fromValueMay >=> \case
                        Just (pfx :: Text) | not (Text.null pfx) ->
                            tryPath path (Just (Text.unpack pfx))
                        _ -> tryPath path Nothing
            Nothing ->
                throwError $ "__nixPath must be a list of attr sets"
                    ++ " with 'path' elements, but saw: " ++ show s

    tryPath p (Just n) | n':ns <- splitDirectories name, n == n' =
        nixFilePath $ p <///> joinPath ns
    tryPath p _ = nixFilePath $ p <///> name
