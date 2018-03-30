{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Nix.Monad.Instance where

import           Control.Monad
import           Control.Monad.Fix
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import           Data.Fix
import           Data.IORef
import qualified Data.Map.Lazy as Map
import qualified Data.Text as Text
import           Nix.Eval
import           Nix.Monad
import           Nix.Parser
import           Nix.Scope
import           Nix.Utils
import           System.Environment
import           System.Exit (ExitCode (ExitSuccess))
import           System.FilePath
import           System.Process (readProcessWithExitCode)

newtype Cyclic m a = Cyclic
    { runCyclic :: ReaderT (NestedScopes (NThunk (Cyclic m))) m a }
    deriving (Functor, Applicative, Monad, MonadFix, MonadIO)

data Deferred m
    = DeferredAction (m (NValue m))
    -- ^ This is closure over the environment where it was created.
    | ComputedValue (NValue m)

instance MonadNix (Cyclic IO) where
    -- jww (2018-03-29): We should use actually stacked scopes here, rather
    -- than constantly merging maps. The number of scope levels will usually
    -- be manageable, but the number of attributes within scopes can be
    -- enormous, making this one of the worst implementations.
    pushScopes s k = Cyclic $ local (combineScopes s) $ do
        scope <- runCyclic currentScope
        traceM $ "scope: " ++ show (() <$ scope)
        runCyclic k

    clearScopes  = Cyclic . local (const (NestedScopes [])) . runCyclic
    currentScope = Cyclic ask

    -- If a variable is being asked for, it's needed in head normal form.
    lookupVar k  = Cyclic $ do
        scope <- ask
        case scopeLookup k scope of
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
                    pushScope (newScope (Map.singleton "__cwd" ref))
                              (evalExpr expr)
        p -> error $ "Unexpected argument to import: " ++ show (() <$ p)

    getEnvVar = forceThunk >=> \case
        NVStr s _ -> do
            mres <- liftIO $ lookupEnv (Text.unpack s)
            return $ case mres of
                Nothing -> NVStr "" mempty
                Just v  -> NVStr (Text.pack v) mempty
        p -> error $ "Unexpected argument to getEnv: " ++ show (() <$ p)
