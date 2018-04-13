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
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Nix.Exec where

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
import qualified Data.HashMap.Lazy as M
import           Data.IORef
import           Data.List
import           Data.List.Split
import           Data.Maybe (mapMaybe)
import           Data.Text (Text)
import qualified Data.Text as Text
import           Nix.Atoms
import           Nix.Context
import           Nix.Effects
import           Nix.Eval
import qualified Nix.Eval as Eval
import           Nix.Expr
import           Nix.Normal
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

type MonadExec e m =
    (Scoped e (NThunk m) m, Framed e m, MonadVar m, MonadFile m,
     MonadEffects m)

nverr :: forall e m a. MonadExec e m => String -> m a
nverr = evalError @(NValue m)

instance MonadExec e m => ConvertValue (NValue m) Bool where
    ofVal = NVConstant . NBool
    wantVal = \case NVConstant (NBool b) -> Just b; _ -> Nothing

instance ConvertValue (NValue m) Int where
    ofVal = NVConstant . NInt . fromIntegral
    wantVal = \case NVConstant (NInt i) -> Just (fromIntegral i); _ -> Nothing

instance ConvertValue (NValue m) Integer where
    ofVal = NVConstant . NInt
    wantVal = \case NVConstant (NInt i) -> Just i; _ -> Nothing

instance ConvertValue (NValue m) Float where
    ofVal = NVConstant . NFloat
    wantVal = \case NVConstant (NFloat f) -> Just f; _ -> Nothing

instance ConvertValue (NValue m) Text where
    ofVal = flip NVStr mempty
    wantVal = \case NVStr t _ -> Just t; _ -> Nothing

instance ConvertValue (NValue m) (Maybe Text) where
    ofVal (Just s) = NVStr s mempty
    ofVal Nothing = NVConstant NNull
    wantVal (NVStr s _) = Just (Just s)
    wantVal (NVConstant NNull) = Just Nothing
    wantVal _ = Nothing

instance ConvertValue (NValue m) (Text, DList Text) where
    ofVal = uncurry NVStr
    wantVal = \case NVStr s p -> Just (s, p); _ -> Nothing

instance ConvertValue (NValue m) (Maybe (Text, DList Text)) where
    ofVal Nothing = NVConstant NNull
    ofVal (Just (s, p)) = NVStr s p
    wantVal = \case
        NVStr s p -> Just (Just (s, p))
        NVConstant NNull -> Just Nothing
        _ -> Nothing

instance ConvertValue (NValue m) [NThunk m] where
    ofVal = NVList
    wantVal = \case NVList l -> Just l; _ -> Nothing

instance ConvertValue (NValue m)
      (AttrSet (NThunk m), AttrSet SourcePos) where
    ofVal (s, p) = NVSet s p
    wantVal = \case NVSet s p -> Just (s, p); _ -> Nothing

instance ConvertValue (NValue m) (AttrSet (NThunk m)) where
    ofVal = flip NVSet M.empty
    wantVal = \case NVSet s _ -> Just s; _ -> Nothing

instance MonadExec e m => MonadThunk (NValue m) (NThunk m) m where
    thunk = fmap coerce . buildThunk
    force = forceThunk . coerce
    value = coerce . valueRef

instance MonadExec e m => MonadEval (NValue m) m where
    freeVariable var =
        nverr $ "Undefined variable '" ++ Text.unpack var ++ "'"

    evalCurPos = do
        Compose (Ann (SrcSpan delta _) _):_ <-
            asks (mapMaybe (either (const Nothing) Just)
                 . view @_ @Frames hasLens)
        return $ posFromSourcePos delta

    evalConstant    = pure . NVConstant
    evalString      = pure . uncurry NVStr
    evalLiteralPath = fmap NVPath . makeAbsolutePath
    evalEnvPath     = fmap NVPath . findEnvPath
    evalUnary       = execUnaryOp
    evalBinary      = execBinaryOp

    evalWith scope body = do
        -- The scope is deliberately wrapped in a thunk here, since it is
        -- evaluated each time a name is looked up within the weak scope, and
        -- we want to be sure the action it evaluates is to force a thunk, so
        -- its value is only computed once.
        traceM "Evaluating with scope"
        s <- thunk scope
        pushWeakScope ?? body $ force s $ \v -> case wantVal v of
            Just (s :: AttrSet (NThunk m)) -> do
                traceM $ "Scope is: " ++ show (void s)
                pure s
            _ -> nverr $ "scope must be a set in with statement, but saw: "
                    ++ show v

    evalIf c t f = case wantVal c of
        Just b -> if b then t else f
        _ -> nverr $ "condition must be a boolean: "++ show c

    evalAssert c body =  case wantVal c of
        Just b -> if b then body else nverr "assertion failed"
        _ -> nverr $ "assertion condition must be boolean, but saw: "
                ++ show c

    evalApp = callFunc
    evalAbs = (pure .) . NVClosure

    evalError = throwError

    type MText (NValue m) = (Text, DList Text)

    wrapMText   = return . (, mempty)
    unwrapMText = return . fst

    embedMText   = return . uncurry NVStr
    projectMText = \case
        NVConstant NNull -> return $ Just Nothing
        v -> fmap (Just . Just) . valueText True =<< normalForm v

infixl 1 `callFunc`
callFunc :: MonadExec e m => NValue m -> m (NValue m) -> m (NValue m)
callFunc fun arg = case fun of
    NVClosure _ f -> do
        traceM "callFunc:NVFunction"
        f arg
    NVBuiltin name f -> do
        traceM $ "callFunc:NVBuiltin " ++ name
        f =<< thunk arg
    s@(NVSet m _) | Just f <- M.lookup "__functor" m -> do
        traceM "callFunc:__functor"
        force f $ \f' -> f' `callFunc` pure s >>= \g' -> g' `callFunc` arg
    x -> throwError $ "Attempt to call non-function: " ++ show x

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
    :: forall e m. (MonadExec e m, MonadEval (NValue m) m)
    => NBinaryOp -> NValue m -> m (NValue m) -> m (NValue m)

execBinaryOp NOr larg rarg = case larg of
    NVConstant (NBool l) -> if l
        then mkBoolV True
        else rarg >>= \case
            NVConstant (NBool r) -> mkBoolV r
            v -> throwError $ "operator `||`: left argument: boolean expected, got " ++ show v
    v -> throwError $ "operator `||`: right argument: boolean expected, got " ++ show v

execBinaryOp NAnd larg rarg = case larg of
    NVConstant (NBool l) -> if l
        then rarg >>= \case
            NVConstant (NBool r) -> mkBoolV r
            v -> throwError $ "operator `&&`: left argument: boolean expected, got " ++ show v
        else mkBoolV False
    v -> throwError $ "operator `&&`: right argument: boolean expected, got " ++ show v

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
                pure . ofVal $             li `intF`               ri
            (NInt   li, NFloat rf) ->
                pure . ofVal $ fromInteger li `floatF`             rf
            (NFloat lf, NInt   ri) ->
                pure . ofVal $             lf `floatF` fromInteger ri
            (NFloat lf, NFloat rf) ->
                pure . ofVal $             lf `floatF`             rf
            _ -> nverr unsupportedTypes

        nverr = evalError @(NValue m)

    case (lval, rval) of
        (NVConstant lc, NVConstant rc) -> case (op, lc, rc) of
            (NEq,  _, _)   -> ofVal <$> valueEq lval rval
            (NNEq, _, _)   -> ofVal . not <$> valueEq lval rval
            (NLt,  l, r)   -> pure . ofVal $ l <  r
            (NLte, l, r)   -> pure . ofVal $ l <= r
            (NGt,  l, r)   -> pure . ofVal $ l >  r
            (NGte, l, r)   -> pure . ofVal $ l >= r
            (NAnd,  _, _)  -> nverr "should be impossible: && is handled above"
            (NOr,   _, _)  -> nverr "should be impossible: || is handled above"
            (NPlus,  l, r) -> numBinOp (+) l r
            (NMinus, l, r) -> numBinOp (-) l r
            (NMult,  l, r) -> numBinOp (*) l r
            (NDiv,   l, r) -> numBinOp' div (/) l r
            (NImpl, NBool l, NBool r) -> pure . ofVal $ not l || r
            _ -> nverr unsupportedTypes

        (NVStr ls lc, NVStr rs rc) -> case op of
            NPlus -> pure $ NVStr (ls `mappend` rs) (lc `mappend` rc)
            NEq   -> ofVal <$> valueEq lval rval
            NNEq  -> ofVal . not <$> valueEq lval rval
            NLt   -> pure . ofVal $ ls <  rs
            NLte  -> pure . ofVal $ ls <= rs
            NGt   -> pure . ofVal $ ls >  rs
            NGte  -> pure . ofVal $ ls >= rs
            _ -> nverr unsupportedTypes

        (NVStr _ _, NVConstant NNull) -> case op of
            NEq   -> ofVal <$> valueEq lval (NVStr "" mempty)
            NNEq  -> ofVal . not <$> valueEq lval (NVStr "" mempty)
            _ -> nverr unsupportedTypes

        (NVConstant NNull, NVStr _ _) -> case op of
            NEq   -> ofVal <$> valueEq (NVStr "" mempty) rval
            NNEq  -> ofVal . not <$> valueEq (NVStr "" mempty) rval
            _ -> nverr unsupportedTypes

        (NVSet ls lp, NVSet rs rp) -> case op of
            NUpdate -> pure $ NVSet (rs `M.union` ls) (rp `M.union` lp)
            NEq     -> ofVal <$> valueEq lval rval
            NNEq    -> ofVal . not <$> valueEq lval rval
            _ -> nverr unsupportedTypes

        (NVList ls, NVList rs) -> case op of
            NConcat -> pure $ NVList $ ls ++ rs
            NEq     -> ofVal <$> valueEq lval rval
            NNEq    -> ofVal . not <$> valueEq lval rval
            _ -> nverr unsupportedTypes

        (NVList ls, NVConstant NNull) -> case op of
            NConcat -> pure $ NVList ls
            NEq     -> ofVal <$> valueEq lval (NVList [])
            NNEq    -> ofVal . not <$> valueEq lval (NVList [])
            _ -> nverr unsupportedTypes

        (NVConstant NNull, NVList rs) -> case op of
            NConcat -> pure $ NVList rs
            NEq     -> ofVal <$> valueEq (NVList []) rval
            NNEq    -> ofVal . not <$> valueEq (NVList []) rval
            _ -> nverr unsupportedTypes

        (NVPath p, NVStr s _) -> case op of
            -- jww (2018-04-13): Do we need to make the path absolute here?
            NEq   -> pure $ ofVal $ p == Text.unpack s
            NNEq  -> pure $ ofVal $ p /= Text.unpack s
            NPlus -> NVPath <$> makeAbsolutePath (p `mappend` Text.unpack s)
            _ -> nverr unsupportedTypes

        (NVPath ls, NVPath rs) -> case op of
            NPlus -> NVPath <$> makeAbsolutePath (ls ++ rs)
            _ -> nverr unsupportedTypes

        _ -> nverr unsupportedTypes

newtype Lazy m a = Lazy
    { runLazy :: ReaderT (Context (Lazy m) (NThunk (Lazy m))) m a }
    deriving (Functor, Applicative, Monad, MonadFix, MonadIO,
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

instance (MonadFix m, MonadThrow m, MonadIO m) => MonadEffects (Lazy m) where
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
            pure $ cwd </> origPath
        liftIO $ removeDotDotIndirections <$> canonicalizePath absPath

    findEnvPath name = foldNixPath Nothing go >>= \case
        Nothing   -> throwError $ "file '" ++ name
                        ++ "' was not found in the Nix search path"
                        ++ " (add it using $NIX_PATH or -I)"
        Just path -> return path
      where
        go p@(Just _) _ _ = pure p
        go Nothing p Nothing = do
            traceM $ "[p] = " ++ p
            traceM $ "name = " ++ name
            traceM $ "cand = " ++ p </> name
            nixFilePath $ p </> name
        go Nothing p (Just n)
            | n':ns <- splitDirectories name, n == n' = do
            traceM $ "[n, p] = " ++ n ++ ", " ++ p
            traceM $ "name = " ++ name
            traceM $ "cand = " ++ p </> joinPath ns
            nixFilePath $ p </> joinPath ns
        go _ _ _ = return Nothing

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
                    Success v -> framedEvalExpr eval v
            err -> throwError $ "nix-instantiate failed: " ++ show err

runLazyM :: MonadIO m => Lazy m a -> m a
runLazyM = flip runReaderT newContext . runLazy

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
    pure $ if isDir
        then p </> "default.nix"
        else p

nixFilePath :: (MonadEffects m, MonadIO m) => FilePath -> m (Maybe FilePath)
nixFilePath path = do
    path <- makeAbsolutePath path
    traceM $ "path = " ++ path
    exists <- liftIO $ doesDirectoryExist path
    traceM $ "exists = " ++ show exists
    path' <-
        if exists
        then makeAbsolutePath $ path </> "default.nix"
        else return path
    traceM $ "path' = " ++ path'
    exists <- liftIO $ doesFileExist path'
    traceM $ "exists = " ++ show exists
    return $ if exists then Just path' else Nothing

foldNixPath :: forall e m r.
                (Scoped e (NThunk m) m, MonadEffects m,
                 Framed e m, MonadThrow m, MonadVar m, MonadFile m)
            => r -> (r -> FilePath -> Maybe String -> m r) -> m r
foldNixPath z f = do
    mres <- lookupVar @_ @(NThunk m) "__includes"
    dirs <- case mres of
        Nothing -> return []
        Just v -> force v $ \case
            NVList xs -> forM xs $ flip force $ \case
                NVStr s _ -> pure s
                _ -> error "impossible"
            _ -> error "impossible"
    menv <- getEnvVar "NIX_PATH"
    foldM go z $ dirs ++ case menv of
        Nothing -> []
        Just str -> Text.splitOn ":" (Text.pack str)
  where
    go acc x = case Text.splitOn "=" x of
        [p]    -> f acc (Text.unpack p) Nothing
        [n, p] -> f acc (Text.unpack p) (Just (Text.unpack n))
        _ -> throwError $ "Unexpected entry in NIX_PATH: " ++ show x
