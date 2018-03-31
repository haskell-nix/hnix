{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Nix.Monad.Instance where

import           Control.Monad
import           Control.Monad.Fix
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import qualified Data.ByteString as BS
import           Data.Fix
import           Data.Functor.Compose
import           Data.IORef
import qualified Data.Map.Lazy as Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           Nix.Eval
import           Nix.Expr
import           Nix.Monad
import           Nix.Parser
import           Nix.Pretty
import           Nix.Scope
import           Nix.Utils
import           System.Environment
import           System.Exit (ExitCode (ExitSuccess))
import           System.FilePath
import           System.Process (readProcessWithExitCode)
import           Text.PrettyPrint.ANSI.Leijen hiding ((<$>), (</>))
import           Text.Trifecta.Rendering
import           Text.Trifecta.Result

data Context m = Context
    { scopes :: NScopes m
    , frames :: [Either String (NExprLocF ())]
    }

mapScopes :: (NScopes m -> NScopes m) -> Context m -> Context m
mapScopes f (Context scopes frames) = Context (f scopes) frames

mapFrames :: ([Either String (NExprLocF ())] -> [Either String (NExprLocF ())])
          -> Context m -> Context m
mapFrames f (Context scopes frames) = Context scopes (f frames)

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
    deriving (Functor, Applicative, Monad, MonadFix, MonadIO)

data Deferred m
    = DeferredAction (m (NValue m))
    -- ^ This is closure over the environment where it was created.
    | ComputedValue (NValue m)

instance Show (NScopes (Cyclic IO)) where
    show (Scopes xs) = show xs

instance MonadNix (Cyclic IO) where
    newtype NScopes (Cyclic IO) =
        Scopes { getS :: [Scope (NThunk (Cyclic IO))] }

    pushScopes (Scopes s) =
        Cyclic . local (mapScopes (Scopes . (s ++) . getS))
               . runCyclic

    pushScope s =
        Cyclic . local (mapScopes (Scopes . (Scope s False:) . getS))
               . runCyclic
    pushWeakScope s =
        Cyclic . local (mapScopes (Scopes . (Scope s True:) . getS))
               . runCyclic

    clearScopes  =
        Cyclic . local (mapScopes (Scopes . const [] . getS)) . runCyclic
    currentScope = Cyclic $ scopes <$> ask

    withExprContext expr =
        Cyclic . local (mapFrames (Right expr :)) . runCyclic
    withStringContext str =
        Cyclic . local (mapFrames (Left str :)) . runCyclic

    throwError str = Cyclic $ do
        context <- reverse . frames <$> ask
        infos   <- liftIO $ mapM renderFrame context
        error $ unlines (infos ++ ["hnix: "++ str])

    -- If a variable is being asked for, it's needed in head normal form.
    lookupVar k  = Cyclic $ do
        env <- ask
        case scopeLookup k (getS (scopes env)) of
            Nothing -> return Nothing
            Just v  -> runCyclic $ Just <$> forceThunk v

    addPath path = liftIO $ do
        (exitCode, out, _) <-
            readProcessWithExitCode "nix-store" ["--add", path] ""
        case exitCode of
          ExitSuccess -> return $ StorePath out
          _ -> error $ "No such file or directory: " ++ show path

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
                scope <- currentScope
                traceM $ "Forcing thunk in scope: " ++ show scope
                value <- action
                traceM $ "Forcing thunk computed: " ++ show (() <$ value)
                liftIO $ writeIORef ref (ComputedValue value)
                return value

instance MonadNixEnv (Cyclic IO) where
    -- jww (2018-03-29): Cache which files have been read in.
    importFile = forceThunk >=> \case
        NVLiteralPath path -> do
            mres <- lookupVar "__cwd"
            path' <- case mres of
                Nothing  -> do
                    traceM "No known current directory"
                    return path
                Just dir -> normalForm dir >>= \case
                    Fix (NVLiteralPath dir') -> do
                        traceM $ "Current directory for import is: "
                            ++ show dir'
                        return $ dir' </> path
                    x -> error $ "How can the current directory be: " ++ show x
            traceM $ "Importing file " ++ path'
            eres <- Cyclic $ parseNixFile path'
            case eres of
                Failure err  -> error $ "Parse failed: " ++ show err
                Success expr -> do
                    ref <- buildThunk $ return $
                        NVLiteralPath $ takeDirectory path'
                    -- Use this cookie so that when we evaluate the next
                    -- import, we'll remember which directory its containing
                    -- file was in.
                    pushScope (Map.singleton "__cwd" ref) (evalExpr expr)
        p -> error $ "Unexpected argument to import: " ++ show (() <$ p)

    getEnvVar = forceThunk >=> \case
        NVStr s _ -> do
            mres <- liftIO $ lookupEnv (Text.unpack s)
            return $ case mres of
                Nothing -> NVStr "" mempty
                Just v  -> NVStr (Text.pack v) mempty
        p -> error $ "Unexpected argument to getEnv: " ++ show (() <$ p)

runCyclicIO :: Cyclic IO a -> IO a
runCyclicIO = flip runReaderT (Context (Scopes []) []) . runCyclic
