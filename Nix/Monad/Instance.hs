{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Nix.Monad.Instance where

import           Control.Monad
import           Control.Monad.Fix
import           Control.Monad.IO.Class
import           Control.Monad.Reader (MonadReader)
import           Control.Monad.Trans.Reader
import qualified Data.ByteString as BS
import           Data.Fix
import           Data.Functor.Compose
import           Data.IORef
import qualified Data.HashMap.Lazy as M
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           Nix.Eval
import           Nix.Expr
import           Nix.Monad
import           Nix.Parser
import           Nix.Pretty
import           Nix.Scope
import           Nix.Utils
import           System.Directory
import           System.Environment
import           System.Exit (ExitCode (ExitSuccess))
import           System.FilePath
import           System.Process (readProcessWithExitCode)
import           Text.PrettyPrint.ANSI.Leijen hiding ((<$>), (</>))
import           Text.Trifecta.Rendering
import           Text.Trifecta.Result

data Context m = Context
    { scopes :: Scopes (NThunk m)
    , frames :: Frames
    }

renderLocation :: MonadIO m => SrcSpan -> Doc -> m Doc
renderLocation (SrcSpan beg@(Directed path _ _ _ _) end) msg = do
    contents <- liftIO $ BS.readFile (Text.unpack (Text.decodeUtf8 path))
    return $ explain (addSpan beg end (rendered beg contents))
                     (Err (Just msg) [] mempty [])
renderLocation (SrcSpan beg end) msg =
    return $ explain (addSpan beg end emptyRendering)
                     (Err (Just msg) [] mempty [])

renderFrame :: MonadIO m => Either String (NExprLocF ()) -> m String
renderFrame (Left str) = return str
renderFrame (Right (Compose (Ann ann expr))) =
    show <$> renderLocation ann
        (prettyNix (Fix (const (Fix (NSym "<?>")) <$> expr)))

newtype Cyclic m a = Cyclic
    { runCyclic :: ReaderT (Context (Cyclic m)) m a }
    deriving (Functor, Applicative, Monad, MonadFix, MonadIO,
              MonadReader (Context (Cyclic m)))

data Deferred m
    = DeferredAction (m (NValue m))
    -- ^ This is closure over the environment where it was created.
    | ComputedValue (NValue m)

instance MonadNix (Cyclic IO) where
    addPath path = liftIO $ do
        (exitCode, out, _) <-
            readProcessWithExitCode "nix-store" ["--add", path] ""
        case exitCode of
          ExitSuccess -> do
            let dropTrailingLinefeed p = take (length p - 1) p
            return $ StorePath $ dropTrailingLinefeed out
          _ -> error $ "No such file or directory: " ++ show path

    makeAbsolutePath p = if isAbsolute p then pure p else do
        cwd <- do
            mres <- lookupVar @_ @(NThunk (Cyclic IO)) "__cwd"
            case mres of
                Nothing -> liftIO getCurrentDirectory
                Just v -> forceThunk v >>= \case
                    NVLiteralPath s -> return s
                    v -> throwError $ "when resolving relative path,"
                            ++ " __cwd is in scope,"
                            ++ " but is not a path; it is: " ++ show (void v)
        liftIO $ canonicalizePath $ cwd </> p

    data NThunk (Cyclic IO) = NThunkIO (IORef (Deferred (Cyclic IO)))

    valueRef value =
        liftIO $ NThunkIO <$> newIORef (ComputedValue value)

    buildThunk action =
        liftIO $ NThunkIO <$> newIORef (DeferredAction action)

    forceThunk (NThunkIO ref) = do
        eres <- liftIO $ readIORef ref
        case eres of
            ComputedValue value -> return value
            DeferredAction action -> do
                scope <- currentScopes @_ @(NThunk (Cyclic IO))
                traceM $ "Forcing thunk in scope: " ++ show scope
                value <- action
                traceM $ "Forcing thunk computed: " ++ show (void value)
                liftIO $ writeIORef ref (ComputedValue value)
                return value

    throwError str = Cyclic $ do
        context <- reverse . frames <$> ask
        infos   <- liftIO $ mapM renderFrame context
        error $ unlines (infos ++ ["hnix: "++ str])

instance Has (Context m) (Scopes (NThunk m)) where
    hasLens f (Context x y) = flip Context y <$> f x

instance Has (Context m) Frames where
    hasLens f (Context x y) = Context x <$> f y

instance MonadNixEnv (Cyclic IO) where
    -- jww (2018-03-29): Cache which files have been read in.
    importFile = forceThunk >=> \case
        NVLiteralPath path -> do
            mres <- lookupVar @(Context (Cyclic IO)) "__cwd"
            path' <- case mres of
                Nothing  -> do
                    traceM "No known current directory"
                    return path
                Just dir -> forceThunk dir >>= normalForm >>= \case
                    Fix (NVLiteralPath dir') -> do
                        traceM $ "Current directory for import is: "
                            ++ show dir'
                        return $ dir' </> path
                    x -> error $ "How can the current directory be: " ++ show x
            traceM $ "Importing file " ++ path'
            withStringContext ("While importing file " ++ show path') $ do
                eres <- Cyclic $ parseNixFile path'
                case eres of
                    Failure err  -> error $ "Parse failed: " ++ show err
                    Success expr -> do
                        ref <- buildThunk $ return $
                            NVLiteralPath $ takeDirectory path'
                        -- Use this cookie so that when we evaluate the next
                        -- import, we'll remember which directory its containing
                        -- file was in.
                        pushScope (M.singleton "__cwd" ref) (evalExpr expr)
        p -> error $ "Unexpected argument to import: " ++ show (void p)

    getEnvVar = forceThunk >=> \case
        NVStr s _ -> do
            mres <- liftIO $ lookupEnv (Text.unpack s)
            return $ case mres of
                Nothing -> NVStr "" mempty
                Just v  -> NVStr (Text.pack v) mempty
        p -> error $ "Unexpected argument to getEnv: " ++ show (void p)

runCyclicIO :: Cyclic IO a -> IO a
runCyclicIO = flip runReaderT (Context emptyScopes []) . runCyclic
