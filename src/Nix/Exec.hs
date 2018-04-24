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
import           Control.Monad.Trans.Reader (ReaderT(..))
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
import           Data.Typeable
import           Data.Void
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
import           System.Directory
import           System.Environment
import           System.Exit (ExitCode (ExitSuccess))
import           System.FilePath
import qualified System.Info
import           System.Posix.Files
import           System.Process (readProcessWithExitCode)
import           Text.PrettyPrint.ANSI.Leijen (text)

type MonadNix e m =
    (Scoped e (NThunk m) m, Framed e m, Typeable m, MonadVar m,
     MonadEffects m, MonadFix m, MonadCatch m, Alternative m)

nverr :: forall e m a. MonadNix e m => String -> m a
nverr = evalError @(NValue m)

instance MonadNix e m => MonadThunk (NValue m) (NThunk m) m where
    thunk = fmap coerce . buildThunk
    force = forceThunk . coerce
    value = coerce . valueRef

currentPos :: Framed e m => m SrcSpan
currentPos = do
    frames <- asks (view @_ @Frames hasLens)
    let EvaluatingExpr (Fix (Compose (Ann span _))) : _ =
            mapMaybe (fromFrame . frame) frames
    return span

instance MonadNix e m => MonadEval (NValue m) m where
    freeVariable var =
        nverr $ "Undefined variable '" ++ Text.unpack var ++ "'"

    evalCurPos = do
        scope <- currentScopes
        span  <- currentPos
        SrcSpan delta _ <- currentPos
        changeProvenance scope (\_ -> NSym_ span "__curPos") <$> toValue delta

    evaledSym name val = do
        span <- currentPos
        pure $ provenanceContext (NSym_ span name) val

    evalConstant c = do
        scope <- currentScopes
        span  <- currentPos
        pure $ nvConstantP (Provenance scope (NConstant_ span c) []) c

    evalString s d = do
        scope <- currentScopes
        span  <- currentPos
        -- jww (2018-04-22): Determine full provenance for the string?
        pure $ nvStrP (Provenance scope (NStr_ span (DoubleQuoted [Plain s]))
                                  []) s d

    evalLiteralPath p = do
        scope <- currentScopes
        span  <- currentPos
        fmap (nvPathP (Provenance scope (NLiteralPath_ span p) []))
                      (makeAbsolutePath p)

    evalEnvPath p = do
        scope <- currentScopes
        span  <- currentPos
        fmap (nvPathP (Provenance scope (NEnvPath_ span p) []))
             (findEnvPath p)

    evalUnary op arg = do
        scope <- currentScopes
        span  <- currentPos
        execUnaryOp scope span op arg

    evalBinary op larg rarg = do
        scope <- currentScopes
        span  <- currentPos
        execBinaryOp scope span op larg rarg

    evalWith c b = do
        span <- currentPos
        -- jww (2018-04-23): What about the arguments to with? All this
        -- preserves right now is the location.
        provenanceContext (NWith_ span Nothing Nothing)
            <$> evalWithAttrSet c b

    evalIf c t f = do
        scope <- currentScopes
        span  <- currentPos
        fromValue c >>= \b ->
            if b
            then changeProvenance scope
                (\t -> NIf_ span (Just c) (Just t) Nothing) <$> t
            else changeProvenance scope
                (\f -> NIf_ span (Just c) Nothing (Just f)) <$> f

    evalAssert c body = fromValue c >>= \b ->
        if b
        then do
            scope <- currentScopes
            span  <- currentPos
            changeProvenance scope
                (\b -> NAssert_ span (Just c) (Just b)) <$> body
        else nverr $ "assertion failed: " ++ show c

    evalApp f x = do
        span <- currentPos
        provenanceContext (NBinary_ span NApp (Just f) Nothing)
            <$> callFunc f x

    evalAbs p b = do
        scope <- currentScopes
        span  <- currentPos
        pure $ nvClosureP (Provenance scope (NAbs_ span (fmap absurd p) Nothing)
                                      []) p b

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
    x -> throwError $ "Attempt to call non-function: " ++ show x

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
            _ -> throwError $ "unsupported argument type for unary operator "
                     ++ show op
        x ->
            -- jww (2018-04-22): Improve error reporting so that instead of
            -- using 'show' to paste the textual form of the value into a
            -- string, we use smarter pattern with typed elements, allowing us
            -- to render specially based on the output device and verbosity
            -- selections.
            throwError $ "argument to unary operator"
                ++ " must evaluate to an atomic type: " ++ show x
  where
    unaryOp = pure . nvConstantP
        (Provenance scope (NUnary_ span op (Just arg)) [])

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
        nvConstantP (Provenance scope (NBinary_ span NOr (Just larg) r) [])
                    (NBool b)

execBinaryOp scope span NAnd larg rarg = fromNix larg >>= \l ->
    if l
    then rarg >>= \rval -> fromNix @Bool rval >>= andOp (Just rval)
    else andOp Nothing False
  where
    andOp r b = pure $
        nvConstantP (Provenance scope (NBinary_ span NAnd (Just larg) r) [])
                    (NBool b)

-- jww (2018-04-08): Refactor so that eval (NBinary ..) *always* dispatches
-- based on operator first
execBinaryOp scope span op lval rarg = do
    rval <- rarg
    let bin :: (Provenance m -> a) -> a
        bin f  = f (Provenance scope (NBinary_ span op (Just lval) (Just rval))
                               [])
        toBool = pure . bin nvConstantP . NBool
    case (lval, rval) of
        (NVConstant lc, NVConstant rc) -> case (op, lc, rc) of
            (NEq,  _, _)       -> toBool =<< valueEq lval rval
            (NNEq, _, _)       -> toBool . not =<< valueEq lval rval
            (NLt,  l, r)       -> toBool $ l <  r
            (NLte, l, r)       -> toBool $ l <= r
            (NGt,  l, r)       -> toBool $ l >  r
            (NGte, l, r)       -> toBool $ l >= r
            (NAnd,  _, _)      -> nverr "should be impossible: && is handled above"
            (NOr,   _, _)      -> nverr "should be impossible: || is handled above"
            (NPlus,  l, r)     -> numBinOp bin (+) l r
            (NMinus, l, r)     -> numBinOp bin (-) l r
            (NMult,  l, r)     -> numBinOp bin (*) l r
            (NDiv,   l, r)     -> numBinOp' bin div (/) l r
            (NImpl,
             NBool l, NBool r) -> toBool $ not l || r
            _                  -> nverr $ unsupportedTypes lval rval

        (NVStr ls lc, NVStr rs rc) -> case op of
            NPlus -> pure $ bin nvStrP (ls `mappend` rs) (lc `mappend` rc)
            NEq   -> toBool =<< valueEq lval rval
            NNEq  -> toBool . not =<< valueEq lval rval
            NLt   -> toBool $ ls <  rs
            NLte  -> toBool $ ls <= rs
            NGt   -> toBool $ ls >  rs
            NGte  -> toBool $ ls >= rs
            _     -> nverr $ unsupportedTypes lval rval

        (NVStr _ _, NVConstant NNull) -> case op of
            NEq  -> toBool =<< valueEq lval (nvStr "" mempty)
            NNEq -> toBool . not =<< valueEq lval (nvStr "" mempty)
            _    -> nverr $ unsupportedTypes lval rval

        (NVConstant NNull, NVStr _ _) -> case op of
            NEq  -> toBool =<< valueEq (nvStr "" mempty) rval
            NNEq -> toBool . not =<< valueEq (nvStr "" mempty) rval
            _    -> nverr $ unsupportedTypes lval rval

        (NVSet ls lp, NVSet rs rp) -> case op of
            NUpdate -> pure $ bin nvSetP (rs `M.union` ls) (rp `M.union` lp)
            NEq     -> toBool =<< valueEq lval rval
            NNEq    -> toBool . not =<< valueEq lval rval
            _       -> nverr $ unsupportedTypes lval rval

        (NVSet ls lp, NVConstant NNull) -> case op of
            NUpdate -> pure $ bin nvSetP ls lp
            NEq     -> toBool =<< valueEq lval (nvSet M.empty M.empty)
            NNEq    -> toBool . not =<< valueEq lval (nvSet M.empty M.empty)
            _       -> nverr $ unsupportedTypes lval rval

        (NVConstant NNull, NVSet rs rp) -> case op of
            NUpdate -> pure $ bin nvSetP rs rp
            NEq     -> toBool =<< valueEq (nvSet M.empty M.empty) rval
            NNEq    -> toBool . not =<< valueEq (nvSet M.empty M.empty) rval
            _       -> nverr $ unsupportedTypes lval rval

        (NVList ls, NVList rs) -> case op of
            NConcat -> pure $ bin nvListP $ ls ++ rs
            NEq     -> toBool =<< valueEq lval rval
            NNEq    -> toBool . not =<< valueEq lval rval
            _       -> nverr $ unsupportedTypes lval rval

        (NVList ls, NVConstant NNull) -> case op of
            NConcat -> pure $ bin nvListP ls
            NEq     -> toBool =<< valueEq lval (nvList [])
            NNEq    -> toBool . not =<< valueEq lval (nvList [])
            _       -> nverr $ unsupportedTypes lval rval

        (NVConstant NNull, NVList rs) -> case op of
            NConcat -> pure $ bin nvListP rs
            NEq     -> toBool =<< valueEq (nvList []) rval
            NNEq    -> toBool . not =<< valueEq (nvList []) rval
            _       -> nverr $ unsupportedTypes lval rval

        (NVPath p, NVStr s _) -> case op of
            -- jww (2018-04-13): Do we need to make the path absolute here?
            NEq   -> toBool $ p == Text.unpack s
            NNEq  -> toBool $ p /= Text.unpack s
            NPlus -> bin nvPathP <$> makeAbsolutePath (p `mappend` Text.unpack s)
            _     -> nverr $ unsupportedTypes lval rval

        (NVPath ls, NVPath rs) -> case op of
            NPlus -> bin nvPathP <$> makeAbsolutePath (ls ++ rs)
            _     -> nverr $ unsupportedTypes lval rval

        _ -> nverr $ unsupportedTypes lval rval
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
        _ -> nverr $ unsupportedTypes l r
      where
        toInt   = pure . bin nvConstantP . NInt
        toFloat = pure . bin nvConstantP . NFloat

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

instance (MonadIO m, Monad m) => MonadFile m where
    readFile = liftIO . BS.readFile

instance MonadCatch m => MonadCatch (Lazy m) where
    catch (Lazy (ReaderT m)) f = Lazy $ ReaderT $ \e ->
        catch (m e) ((`runReaderT` e) . runLazy . f)

instance MonadThrow m => MonadThrow (Lazy m) where
    throwM = Lazy . throwM

instance (MonadFix m, MonadCatch m, MonadThrow m, MonadIO m,
          Alternative m, Typeable m)
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
        origPathExpanded <- liftIO $ expandHomePath origPath
        absPath <- if isAbsolute origPathExpanded then pure origPathExpanded else do
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
            pure $ cwd <///> origPathExpanded
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
            Just p -> fromValue @_ @_ @(NThunk (Lazy m)) p >>= \(Path p') -> do
                traceM $ "Current file being evaluated is: " ++ show p'
                return $ takeDirectory p' </> path

        traceM $ "Importing file " ++ path'

        withFrame Info ("While importing file " ++ show path') $ do
            eres <- Lazy $ parseNixFileLoc path'
            case eres of
                Failure err  -> error $ "Parse failed: " ++ show err
                Success expr -> do
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
        ignoreNulls <- case M.lookup "__ignoreNulls" s of
            Nothing -> pure False
            Just v  -> fromNix v
        v' <- normalForm
            =<< toValue @(ValueSet (Lazy m)) . M.fromList
            =<< mapMaybeM
                (\(k, v) -> fmap (k,) <$> case k of
                    "args" -> fmap Just . thunk $
                        toNix =<< fromNix @[Text] v
                    "__ignoreNulls" -> pure Nothing
                    _ -> force v $ \case
                        NVConstant NNull | ignoreNulls -> pure Nothing
                        v' -> fmap Just $ thunk $
                            toNix =<< (Text.pack <$> specialToString v'))
                (M.toList s)
        nixInstantiateExpr $ "derivationStrict " ++ show (prettyNixValue v')
      where
        mapMaybeM :: (a -> Lazy m (Maybe b)) -> [a] -> Lazy m [b]
        mapMaybeM op = foldr f (return [])
          where
              f x xs = do
                  x <- op x
                  case x of
                      Nothing -> xs
                      Just x -> do
                          xs <- xs
                          return $ x:xs

        specialToString :: NValue (Lazy m) -> Lazy m String
        specialToString = \case
            NVConstant (NBool b)
                | b -> pure "1"
                | otherwise -> pure ""
            NVConstant (NInt n)   -> pure $ show n
            NVConstant (NFloat n) -> pure $ show n
            NVConstant (NUri u)   -> pure $ show u
            NVConstant NNull      -> pure ""
            NVList l -> unwords <$> traverse (`force` specialToString) l
            NVStr t _ -> pure $ Text.unpack t
            NVPath p -> unStorePath <$> addPath p
            NVSet s _ | Just p <- M.lookup "outPath" s -> force p specialToString
            v -> throwError $ "Expected a string, but saw: " ++ show v

    nixInstantiateExpr expr = do
        traceM $ "Executing: "
            ++ show ["nix-instantiate", "--eval", "--expr ", expr]
        (exitCode, out, _) <-
            liftIO $ readProcessWithExitCode "nix-instantiate"
                [ "--eval", "--expr", expr] ""
        case exitCode of
            ExitSuccess ->
                case parseNixTextLoc (Text.pack out) of
                    Failure err ->
                        throwError $ "Error parsing output of nix-instantiate: "
                            ++ show err
                    Success v -> evalExprLoc v
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
             adi (addTracing phi) (raise addStackFrames) expr
        else adi phi addStackFrames expr
  where
    phi = Eval.eval @_ @(NValue m) @(NThunk m) @m . annotated . getCompose
    raise k f x = ReaderT $ \e -> k (\t -> runReaderT (f t) e) x
