{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE CPP #-}
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
import           Control.Monad.Reader
import           Control.Monad.State.Strict
import           Control.Monad.Trans.Reader (ReaderT(..))
import           Control.Monad.Trans.State.Strict (StateT(..))
import qualified Data.ByteString as BS
import           Data.Coerce
import           Data.Fix
import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as M
import           Data.IORef
import           Data.List
import           Data.List.Split
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Typeable
import           Nix.Atoms
import           Nix.Context
import           Nix.Convert
import           Nix.Effects
import           Nix.Eval as Eval
import           Nix.Expr
import           Nix.Frames
import           Nix.Normal
import           Nix.Options
import           Nix.Parser
import           Nix.Pretty
import           Nix.Render
import           Nix.Scope
import           Nix.Thunk
import           Nix.Utils
import           Nix.Value
import           System.Console.Haskeline.MonadException hiding (catch)
import           System.Directory
import           System.Environment
import           System.Exit (ExitCode (ExitSuccess))
import           System.FilePath
import qualified System.Info
import           System.Posix.Files
import           System.Process (readProcessWithExitCode)
import           Text.PrettyPrint.ANSI.Leijen (text)
import qualified Text.PrettyPrint.ANSI.Leijen as P

#ifdef MIN_VERSION_ghc_datasize
#if MIN_VERSION_ghc_datasize(0,2,0) && __GLASGOW_HASKELL__ >= 804
import           GHC.DataSize
#endif
#endif

type MonadNix e m =
    (Scoped e (NThunk m) m, Framed e m, Has e SrcSpan, Has e Options,
     Typeable m, MonadVar m, MonadEffects m, MonadFix m, MonadCatch m,
     Alternative m)

data ExecFrame m = Assertion SrcSpan (NValue m)
    deriving (Show, Typeable)

instance Typeable m => Exception (ExecFrame m)

nverr :: forall s e m a. (MonadNix e m, Exception s) => s -> m a
nverr = evalError @(NValue m)

currentPos :: forall e m. (MonadReader e m, Has e SrcSpan) => m SrcSpan
currentPos = asks (view @e @SrcSpan hasLens)

wrapExprLoc :: SrcSpan -> NExprLocF r -> NExprLoc
wrapExprLoc span x = Fix (Fix (NSym_ span "<?>") <$ x)

instance MonadNix e m => MonadThunk (NValue m) (NThunk m) m where
    thunk mv = do
        opts :: Options <- asks (view hasLens)

        if thunks opts
            then do
                frames <- asks (view @_ @Frames hasLens)

                -- Gather the current evaluation context at the time of thunk
                -- creation, and record it along with the thunk.
                let go (fromException ->
                            Just (EvaluatingExpr scope
                                     (Fix (Compose (Ann span e))))) =
                        let e' = Compose (Ann span (Nothing <$ e))
                        in [Provenance scope e']
                    go _ = []
                    ps = concatMap (go . frame) frames

                fmap (NThunk ps . coerce) . buildThunk $ mv
            else
                fmap (NThunk [] . coerce) . buildThunk $ mv

    -- The ThunkLoop exception is thrown as an exception with MonadThrow,
    -- which does not capture the current stack frame information to provide
    -- it in a NixException, so we catch and re-throw it here using
    -- 'throwError' from Frames.hs.
    force (NThunk ps t) f = catch go (throwError @ThunkLoop)
      where
        go = case ps of
            [] -> forceThunk t f
            Provenance scope e@(Compose (Ann span _)):_ ->
                withFrame Info (ForcingExpr scope (wrapExprLoc span e))
                    (forceThunk t f)

    value = NThunk [] . coerce . valueRef

{-
prov :: MonadNix e m
     => (NValue m -> Provenance m) -> NValue m -> m (NValue m)
prov p v = do
    opts :: Options <- asks (view hasLens)
    pure $ if values opts
           then addProvenance p v
           else v
-}

instance MonadNix e m => MonadEval (NValue m) m where
    freeVariable var =
        nverr $ ErrorCall $ "Undefined variable '" ++ Text.unpack var ++ "'"

    evalCurPos = do
        scope <- currentScopes
        span@(SrcSpan delta _) <- currentPos
        addProvenance (\_ -> Provenance scope (NSym_ span "__curPos"))
            <$> toValue delta

    evaledSym name val = do
        scope <- currentScopes
        span <- currentPos
        pure $ addProvenance (const $ Provenance scope (NSym_ span name)) val

    evalConstant c = do
        scope <- currentScopes
        span  <- currentPos
        pure $ nvConstantP (Provenance scope (NConstant_ span c)) c

    evalString = assembleString >=> \case
        Just (s, c) -> do
            scope <- currentScopes
            span  <- currentPos
            pure $ nvStrP (Provenance scope
                           (NStr_ span (DoubleQuoted [Plain s]))) s c
        Nothing -> nverr $ ErrorCall $ "Failed to assemble string"

    evalLiteralPath p = do
        scope <- currentScopes
        span  <- currentPos
        nvPathP (Provenance scope (NLiteralPath_ span p)) <$> makeAbsolutePath p

    evalEnvPath p = do
        scope <- currentScopes
        span  <- currentPos
        nvPathP (Provenance scope (NEnvPath_ span p)) <$> findEnvPath p

    evalUnary op arg = do
        scope <- currentScopes
        span  <- currentPos
        execUnaryOp scope span op arg

    evalBinary op larg rarg = do
        scope <- currentScopes
        span  <- currentPos
        execBinaryOp scope span op larg rarg

    evalWith c b = do
        scope <- currentScopes
        span <- currentPos
        addProvenance (\b -> Provenance scope (NWith_ span Nothing (Just b)))
            <$> evalWithAttrSet c b

    evalIf c t f = do
        scope <- currentScopes
        span  <- currentPos
        fromValue c >>= \b ->
            if b
            then addProvenance (\t -> Provenance scope (NIf_ span (Just c) (Just t) Nothing)) <$> t
            else addProvenance (\f -> Provenance scope (NIf_ span (Just c) Nothing (Just f))) <$> f

    evalAssert c body = fromValue c >>= \b -> do
        span  <- currentPos
        if b
            then do
                scope <- currentScopes
                addProvenance (\b -> Provenance scope (NAssert_ span (Just c) (Just b))) <$> body
            else nverr $ Assertion span c

    evalApp f x = do
        scope <- currentScopes
        span <- currentPos
        addProvenance (const $ Provenance scope (NBinary_ span NApp (Just f) Nothing))
            <$> callFunc f x

    evalAbs p k = do
        scope <- currentScopes
        span  <- currentPos
        pure $ nvClosureP (Provenance scope (NAbs_ span (Nothing <$ p) Nothing))
            (void p) (\arg -> snd <$> k arg (\_ b -> ((),) <$> b))

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
    x -> throwError $ ErrorCall $ "Attempt to call non-function: " ++ show x

execUnaryOp :: (Framed e m, MonadVar m)
            => Scopes m (NThunk m) -> SrcSpan -> NUnaryOp -> NValue m
            -> m (NValue m)
execUnaryOp scope span op arg = do
    traceM "NUnary"
    case arg of
        NVConstant c -> case (op, c) of
            (NNeg, NInt   i) -> unaryOp $ NInt   (-i)
            (NNeg, NFloat f) -> unaryOp $ NFloat (-f)
            (NNot, NBool  b) -> unaryOp $ NBool  (not b)
            _ -> throwError $ ErrorCall $
                "unsupported argument type for unary operator " ++ show op
        x -> throwError $ ErrorCall $ "argument to unary operator"
                ++ " must evaluate to an atomic type: " ++ show x
  where
    unaryOp = pure . nvConstantP (Provenance scope (NUnary_ span op (Just arg)))

execBinaryOp
    :: forall e m. (MonadNix e m, MonadEval (NValue m) m)
    => Scopes m (NThunk m)
    -> SrcSpan
    -> NBinaryOp
    -> NValue m
    -> m (NValue m)
    -> m (NValue m)

execBinaryOp scope span NOr larg rarg = fromNix larg >>= \l ->
    if l
    then orOp Nothing True
    else rarg >>= \rval -> fromNix @Bool rval >>= orOp (Just rval)
  where
    orOp r b = pure $
        nvConstantP (Provenance scope (NBinary_ span NOr (Just larg) r)) (NBool b)

execBinaryOp scope span NAnd larg rarg = fromNix larg >>= \l ->
    if l
    then rarg >>= \rval -> fromNix @Bool rval >>= andOp (Just rval)
    else andOp Nothing False
  where
    andOp r b = pure $
        nvConstantP (Provenance scope (NBinary_ span NAnd (Just larg) r)) (NBool b)

execBinaryOp scope span op lval rarg = do
    rval <- rarg
    let bin :: (Provenance m -> a) -> a
        bin f  = f (Provenance scope (NBinary_ span op (Just lval) (Just rval)))
        toBool = pure . bin nvConstantP . NBool
    case (lval, rval) of
        (NVConstant lc, NVConstant rc) -> case (op, lc, rc) of
            (NEq,  _, _)       -> toBool =<< valueEq lval rval
            (NNEq, _, _)       -> toBool . not =<< valueEq lval rval
            (NLt,  l, r)       -> toBool $ l <  r
            (NLte, l, r)       -> toBool $ l <= r
            (NGt,  l, r)       -> toBool $ l >  r
            (NGte, l, r)       -> toBool $ l >= r
            (NAnd,  _, _)      ->
                nverr $ ErrorCall "should be impossible: && is handled above"
            (NOr,   _, _)      ->
                nverr $ ErrorCall "should be impossible: || is handled above"
            (NPlus,  l, r)     -> numBinOp bin (+) l r
            (NMinus, l, r)     -> numBinOp bin (-) l r
            (NMult,  l, r)     -> numBinOp bin (*) l r
            (NDiv,   l, r)     -> numBinOp' bin div (/) l r
            (NImpl,
             NBool l, NBool r) -> toBool $ not l || r
            _                  -> nverr $ ErrorCall $ unsupportedTypes lval rval

        (NVStr ls lc, NVStr rs rc) -> case op of
            NPlus -> pure $ bin nvStrP (ls `mappend` rs) (lc `mappend` rc)
            NEq   -> toBool =<< valueEq lval rval
            NNEq  -> toBool . not =<< valueEq lval rval
            NLt   -> toBool $ ls <  rs
            NLte  -> toBool $ ls <= rs
            NGt   -> toBool $ ls >  rs
            NGte  -> toBool $ ls >= rs
            _     -> nverr $ ErrorCall $ unsupportedTypes lval rval

        (NVStr _ _, NVConstant NNull) -> case op of
            NEq  -> toBool =<< valueEq lval (nvStr "" mempty)
            NNEq -> toBool . not =<< valueEq lval (nvStr "" mempty)
            _    -> nverr $ ErrorCall $ unsupportedTypes lval rval

        (NVConstant NNull, NVStr _ _) -> case op of
            NEq  -> toBool =<< valueEq (nvStr "" mempty) rval
            NNEq -> toBool . not =<< valueEq (nvStr "" mempty) rval
            _    -> nverr $ ErrorCall $ unsupportedTypes lval rval

        (NVSet ls lp, NVSet rs rp) -> case op of
            NUpdate -> pure $ bin nvSetP (rs `M.union` ls) (rp `M.union` lp)
            NEq     -> toBool =<< valueEq lval rval
            NNEq    -> toBool . not =<< valueEq lval rval
            _       -> nverr $ ErrorCall $ unsupportedTypes lval rval

        (NVSet ls lp, NVConstant NNull) -> case op of
            NUpdate -> pure $ bin nvSetP ls lp
            NEq     -> toBool =<< valueEq lval (nvSet M.empty M.empty)
            NNEq    -> toBool . not =<< valueEq lval (nvSet M.empty M.empty)
            _       -> nverr $ ErrorCall $ unsupportedTypes lval rval

        (NVConstant NNull, NVSet rs rp) -> case op of
            NUpdate -> pure $ bin nvSetP rs rp
            NEq     -> toBool =<< valueEq (nvSet M.empty M.empty) rval
            NNEq    -> toBool . not =<< valueEq (nvSet M.empty M.empty) rval
            _       -> nverr $ ErrorCall $ unsupportedTypes lval rval

        (NVList ls, NVList rs) -> case op of
            NConcat -> pure $ bin nvListP $ ls ++ rs
            NEq     -> toBool =<< valueEq lval rval
            NNEq    -> toBool . not =<< valueEq lval rval
            _       -> nverr $ ErrorCall $ unsupportedTypes lval rval

        (NVList ls, NVConstant NNull) -> case op of
            NConcat -> pure $ bin nvListP ls
            NEq     -> toBool =<< valueEq lval (nvList [])
            NNEq    -> toBool . not =<< valueEq lval (nvList [])
            _       -> nverr $ ErrorCall $ unsupportedTypes lval rval

        (NVConstant NNull, NVList rs) -> case op of
            NConcat -> pure $ bin nvListP rs
            NEq     -> toBool =<< valueEq (nvList []) rval
            NNEq    -> toBool . not =<< valueEq (nvList []) rval
            _       -> nverr $ ErrorCall $ unsupportedTypes lval rval

        (NVPath p, NVStr s _) -> case op of
            NEq   -> toBool $ p == Text.unpack s
            NNEq  -> toBool $ p /= Text.unpack s
            NPlus -> bin nvPathP <$> makeAbsolutePath (p `mappend` Text.unpack s)
            _     -> nverr $ ErrorCall $ unsupportedTypes lval rval

        (NVPath ls, NVPath rs) -> case op of
            NPlus -> bin nvPathP <$> makeAbsolutePath (ls ++ rs)
            _     -> nverr $ ErrorCall $ unsupportedTypes lval rval

        _ -> case op of
            NEq   -> toBool False
            NNEq  -> toBool True
            _ -> nverr $ ErrorCall $ unsupportedTypes lval rval
  where
    unsupportedTypes :: Show a => a -> a -> String
    unsupportedTypes lval rval =
        "Unsupported argument types for binary operator "
            ++ show op ++ ": " ++ show lval ++ ", " ++ show rval

    numBinOp :: (forall r. (Provenance m -> r) -> r)
             -> (forall a. Num a => a -> a -> a) -> NAtom -> NAtom -> m (NValue m)
    numBinOp bin f = numBinOp' bin f f

    numBinOp' :: (forall r. (Provenance m -> r) -> r)
              -> (Integer -> Integer -> Integer)
              -> (Float -> Float -> Float)
              -> NAtom -> NAtom -> m (NValue m)
    numBinOp' bin intF floatF l r = case (l, r) of
        (NInt   li, NInt   ri) -> toInt   $             li `intF`               ri
        (NInt   li, NFloat rf) -> toFloat $ fromInteger li `floatF`             rf
        (NFloat lf, NInt   ri) -> toFloat $             lf `floatF` fromInteger ri
        (NFloat lf, NFloat rf) -> toFloat $             lf `floatF`             rf
        _ -> nverr $ ErrorCall $ unsupportedTypes l r
      where
        toInt   = pure . bin nvConstantP . NInt
        toFloat = pure . bin nvConstantP . NFloat

coerceToString :: MonadNix e m => NValue m -> m String
coerceToString = \case
    NVConstant (NBool b)
        | b               -> pure "1"
        | otherwise       -> pure ""
    NVConstant (NInt n)   -> pure $ show n
    NVConstant (NFloat n) -> pure $ show n
    NVConstant (NUri u)   -> pure $ show u
    NVConstant NNull      -> pure ""

    NVStr t _ -> pure $ Text.unpack t
    NVPath p  -> unStorePath <$> addPath p
    NVList l  -> unwords <$> traverse (`force` coerceToString) l

    v@(NVSet s _) | Just p <- M.lookup "__toString" s ->
        force p $ (`callFunc` pure v) >=> coerceToString

    NVSet s _ | Just p <- M.lookup "outPath" s ->
        force p coerceToString

    v -> throwError $ ErrorCall $ "Expected a string, but saw: " ++ show v

newtype Lazy m a = Lazy
    { runLazy :: ReaderT (Context (Lazy m) (NThunk (Lazy m)))
                        (StateT (HashMap FilePath NExprLoc) m) a }
    deriving (Functor, Applicative, Alternative, Monad, MonadPlus,
              MonadFix, MonadIO,
              MonadReader (Context (Lazy m) (NThunk (Lazy m))))

instance MonadIO m => MonadVar (Lazy m) where
    type Var (Lazy m) = IORef

    newVar = liftIO . newIORef
    readVar = liftIO . readIORef
    writeVar = (liftIO .) . writeIORef
    atomicModifyVar = (liftIO .) . atomicModifyIORef

instance (MonadIO m, Monad m) => MonadFile m where
    readFile = liftIO . BS.readFile

instance MonadCatch m => MonadCatch (Lazy m) where
    catch (Lazy (ReaderT m)) f = Lazy $ ReaderT $ \e ->
        catch (m e) ((`runReaderT` e) . runLazy . f)

instance MonadThrow m => MonadThrow (Lazy m) where
    throwM = Lazy . throwM

instance MonadException m => MonadException (Lazy m) where
  controlIO f = Lazy $ controlIO $ \(RunIO run) ->
      let run' = RunIO (fmap Lazy . run . runLazy)
      in runLazy <$> f run'

instance (MonadFix m, MonadCatch m, MonadIO m, Alternative m,
          MonadPlus m, Typeable m)
      => MonadEffects (Lazy m) where
    addPath path = do
        (exitCode, out, _) <-
            liftIO $ readProcessWithExitCode "nix-store" ["--add", path] ""
        case exitCode of
          ExitSuccess -> do
            let dropTrailingLinefeed p = take (length p - 1) p
            return $ StorePath $ dropTrailingLinefeed out
          _ -> throwError $ ErrorCall $
                  "addPath: failed: nix-store --add " ++ show path

    makeAbsolutePath origPath = do
        origPathExpanded <- liftIO $ expandHomePath origPath
        absPath <- if isAbsolute origPathExpanded then pure origPathExpanded else do
            cwd <- do
                mres <- lookupVar @_ @(NThunk (Lazy m)) "__cur_file"
                case mres of
                    Nothing -> liftIO getCurrentDirectory
                    Just v -> force v $ \case
                        NVPath s -> return $ takeDirectory s
                        v -> throwError $ ErrorCall $ "when resolving relative path,"
                                ++ " __cur_file is in scope,"
                                ++ " but is not a path; it is: "
                                ++ show v
            pure $ cwd <///> origPathExpanded
        liftIO $ removeDotDotIndirections <$> canonicalizePath absPath

    findEnvPath = findEnvPathM

    pathExists = liftIO . fileExist

    importPath scope origPath = do
        path <- liftIO $ pathToDefaultNixFile origPath
        mres <- lookupVar @(Context (Lazy m) (NThunk (Lazy m)))
                         "__cur_file"
        path' <- case mres of
            Nothing  -> do
                traceM "No known current directory"
                return path
            Just p -> fromValue @_ @_ @(NThunk (Lazy m)) p >>= \(Path p') -> do
                traceM $ "Current file being evaluated is: " ++ show p'
                return $ takeDirectory p' </> path

        traceM $ "Importing file " ++ path'
        withFrame Info (ErrorCall $ "While importing file " ++ show path') $ do
            imports <- Lazy $ ReaderT $ const get
            expr <- case M.lookup path' imports of
                Just expr -> pure expr
                Nothing -> do
                    eres <- Lazy $ parseNixFileLoc path'
                    case eres of
                        Failure err  ->
                            throwError $ ErrorCall . show $
                                text "Parse during import failed:" P.</> err
                        Success expr -> do
                            Lazy $ ReaderT $ const $
                                modify (M.insert origPath expr)
                            pure expr

            let ref = value @_ @_ @(Lazy m) (nvPath path')
            -- Use this cookie so that when we evaluate the next
            -- import, we'll remember which directory its containing
            -- file was in.
            pushScope (M.singleton "__cur_file" ref) $
                pushScope scope $ evalExprLoc expr

    getEnvVar = liftIO . lookupEnv

    getCurrentSystemOS = return $ Text.pack System.Info.os

    -- Invert the conversion done by GHC_CONVERT_CPU in GHC's aclocal.m4
    getCurrentSystemArch = return $ Text.pack $ case System.Info.arch of
      "i386" -> "i686"
      arch -> arch

    listDirectory         = liftIO . System.Directory.listDirectory
    getSymbolicLinkStatus = liftIO . System.Posix.Files.getSymbolicLinkStatus

    derivationStrict = fromValue @(ValueSet (Lazy m)) >=> \s -> do
        nn <- maybe (pure False) fromNix (M.lookup "__ignoreNulls" s)
        s' <- M.fromList <$> mapMaybeM (handleEntry nn) (M.toList s)
        v' <- normalForm =<< toValue @(ValueSet (Lazy m)) s'
        nixInstantiateExpr $ "derivationStrict " ++ show (prettyNValueNF v')
      where
        mapMaybeM :: (a -> Lazy m (Maybe b)) -> [a] -> Lazy m [b]
        mapMaybeM op = foldr f (return [])
          where f x xs = op x >>= \case
                    Nothing -> xs
                    Just x  -> (x:) <$> xs

        handleEntry ignoreNulls (k, v) = fmap (k,) <$> case k of
            -- The `args' attribute is special: it supplies the command-line
            -- arguments to the builder.
            "args"          -> Just <$> convertNix @[Text] v
            "__ignoreNulls" -> pure Nothing
            _               -> force v $ \case
                NVConstant NNull | ignoreNulls -> pure Nothing
                v' -> Just <$> (toNix =<< Text.pack <$> coerceToString v')

    nixInstantiateExpr expr = do
        traceM $ "Executing: "
            ++ show ["nix-instantiate", "--eval", "--expr ", expr]
        (exitCode, out, err) <-
            liftIO $ readProcessWithExitCode "nix-instantiate"
                [ "--eval", "--expr", expr] ""
        case exitCode of
            ExitSuccess -> case parseNixTextLoc (Text.pack out) of
                Failure err ->
                    throwError $ ErrorCall $
                        "Error parsing output of nix-instantiate: " ++ show err
                Success v -> evalExprLoc v
            status ->
                throwError $ ErrorCall $ "nix-instantiate failed: " ++ show status
                    ++ ": " ++ err

    getRecursiveSize =
#ifdef MIN_VERSION_ghc_datasize
#if MIN_VERSION_ghc_datasize(0,2,0) && __GLASGOW_HASKELL__ >= 804
        toNix @Integer <=< fmap fromIntegral . liftIO . recursiveSize
#else
        const $ toNix (0 :: Integer)
#endif
#else
        const $ toNix (0 :: Integer)
#endif

runLazyM :: Options -> MonadIO m => Lazy m a -> m a
runLazyM opts = (`evalStateT` M.empty)
              . (`runReaderT` newContext opts)
              . runLazy

-- | Incorrectly normalize paths by rewriting patterns like @a/b/..@ to @a@.
--   This is incorrect on POSIX systems, because if @b@ is a symlink, its
--   parent may be a different directory from @a@. See the discussion at
--   https://hackage.haskell.org/package/directory-1.3.1.5/docs/System-Directory.html#v:canonicalizePath
removeDotDotIndirections :: FilePath -> FilePath
removeDotDotIndirections = intercalate "/" . go [] . splitOn "/"
    where go s [] = reverse s
          go (_:s) ("..":rest) = go s rest
          go s (this:rest) = go (this:s) rest

expandHomePath :: FilePath -> IO FilePath
expandHomePath ('~' : xs) = flip (++) xs <$> getHomeDirectory
expandHomePath p = return p

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
            throwError $ ErrorCall $ "file '" ++ name
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
                throwError $ ErrorCall $ "__nixPath must be a list of attr sets"
                    ++ " with 'path' elements, but saw: " ++ show s

    tryPath p (Just n) | n':ns <- splitDirectories name, n == n' =
        nixFilePath $ p <///> joinPath ns
    tryPath p _ = nixFilePath $ p <///> name

addTracing :: (MonadNix e m, Has e Options, MonadIO m,
              MonadReader Int n, Alternative n)
           => Alg NExprLocF (m a) -> Alg NExprLocF (n (m a))
addTracing k v = do
    depth <- ask
    guard (depth < 2000)
    local succ $ do
        v'@(Compose (Ann span x)) <- sequence v
        return $ do
            opts :: Options <- asks (view hasLens)
            let rendered =
                    if verbose opts >= Chatty
                    then show (void x)
                    else show (prettyNix (Fix (Fix (NSym "?") <$ x)))
                msg x = "eval: " ++ replicate depth ' ' ++ x
            loc <- renderLocation span (text (msg rendered ++ " ..."))
            liftIO $ putStr $ show loc
            res <- k v'
            liftIO $ putStrLn $ msg (rendered ++ " ...done")
            return res

evalExprLoc :: forall e m. (MonadNix e m, Has e Options, MonadIO m)
            => NExprLoc -> m (NValue m)
evalExprLoc expr = do
    opts :: Options <- asks (view hasLens)
    if tracing opts
        then join . (`runReaderT` (0 :: Int)) $
             adi (addTracing phi)
                 (raise (addStackFrames @(NThunk m) . addSourcePositions))
                 expr
        else adi phi (addStackFrames @(NThunk m) . addSourcePositions) expr
  where
    phi = Eval.eval @_ @(NValue m) @(NThunk m) @m . annotated . getCompose
    raise k f x = ReaderT $ \e -> k (\t -> runReaderT (f t) e) x
