{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module Nix.Effects where

import           Prelude hiding (putStr, putStrLn, print)
import qualified Prelude

import           Control.Monad.Trans
import           Data.Text (Text)
import qualified Data.Text as T
import           Network.HTTP.Client hiding (path)
import           Network.HTTP.Client.TLS
import           Network.HTTP.Types
import           Nix.Expr
import           Nix.Frames
import           Nix.Parser
import           Nix.Render
import           Nix.Utils
import           Nix.Value
import qualified System.Directory as S
import           System.Environment
import           System.Exit
import qualified System.Info
import           System.Process

-- | A path into the nix store
newtype StorePath = StorePath { unStorePath :: FilePath }

class (MonadFile m, MonadStore m, MonadPutStr m, MonadHttp m, MonadEnv m, MonadInstantiate m, MonadExec m, MonadIntrospect m) => MonadEffects m where
    -- | Determine the absolute path of relative path in the current context
    makeAbsolutePath :: FilePath -> m FilePath
    findEnvPath :: String -> m FilePath

    -- | Having an explicit list of sets corresponding to the NIX_PATH
    -- and a file path try to find an existing path
    findPath :: [NThunk m] -> FilePath -> m FilePath

    importPath :: FilePath -> m (NValue m)
    pathToDefaultNix :: FilePath -> m FilePath

    derivationStrict :: NValue m -> m (NValue m)

    traceEffect :: String -> m ()

class Monad m => MonadIntrospect m where
    recursiveSize :: a -> m Word
    default recursiveSize :: (MonadTrans t, MonadIntrospect m', m ~ t m') => a -> m Word
    recursiveSize = lift . recursiveSize

instance MonadIntrospect IO where
    recursiveSize =
#ifdef MIN_VERSION_ghc_datasize
#if MIN_VERSION_ghc_datasize(0,2,0) && __GLASGOW_HASKELL__ >= 804
        recursiveSize
#else
        \_ -> return 0
#endif
#else
        \_ -> return 0
#endif

class Monad m => MonadExec m where
    exec' :: [String] -> m (Either ErrorCall NExprLoc)
    default exec' :: (MonadTrans t, MonadExec m', m ~ t m') => [String] -> m (Either ErrorCall NExprLoc)
    exec' = lift . exec'

instance MonadExec IO where
    exec' = \case
      [] -> return $ Left $ ErrorCall "exec: missing program"
      (prog:args) -> do
        (exitCode, out, _) <-
            liftIO $ readProcessWithExitCode prog args ""
        let t = T.strip (T.pack out)
        let emsg = "program[" ++ prog ++ "] args=" ++ show args
        case exitCode of
          ExitSuccess ->
            if T.null t
            then return $ Left $ ErrorCall $ "exec has no output :" ++ emsg
            else case parseNixTextLoc t of
              Failure err ->
                return $ Left $ ErrorCall $
                    "Error parsing output of exec: " ++ show err ++ " " ++ emsg
              Success v -> return $ Right v
          err -> return $ Left $ ErrorCall $
                    "exec  failed: " ++ show err ++ " " ++ emsg

class Monad m => MonadInstantiate m where
    instantiateExpr :: String -> m (Either ErrorCall NExprLoc)
    default instantiateExpr :: (MonadTrans t, MonadInstantiate m', m ~ t m') => String -> m (Either ErrorCall NExprLoc)
    instantiateExpr = lift . instantiateExpr

instance MonadInstantiate IO where
    instantiateExpr expr = do
        traceM $ "Executing: "
            ++ show ["nix-instantiate", "--eval", "--expr ", expr]
        (exitCode, out, err) <-
            readProcessWithExitCode "nix-instantiate"
                [ "--eval", "--expr", expr] ""
        case exitCode of
            ExitSuccess -> case parseNixTextLoc (T.pack out) of
                Failure e ->
                    return $ Left $ ErrorCall $
                        "Error parsing output of nix-instantiate: " ++ show e
                Success v -> return $ Right v
            status ->
                return $ Left $ ErrorCall $ "nix-instantiate failed: " ++ show status
                    ++ ": " ++ err

pathExists :: MonadFile m => FilePath -> m Bool
pathExists = doesFileExist

class Monad m => MonadEnv m where
    getEnvVar :: String -> m (Maybe String)
    default getEnvVar :: (MonadTrans t, MonadEnv m', m ~ t m') => String -> m (Maybe String)
    getEnvVar = lift . getEnvVar
    getCurrentSystemOS :: m Text
    default getCurrentSystemOS :: (MonadTrans t, MonadEnv m', m ~ t m') => m Text
    getCurrentSystemOS = lift getCurrentSystemOS
    getCurrentSystemArch :: m Text
    default getCurrentSystemArch :: (MonadTrans t, MonadEnv m', m ~ t m') => m Text
    getCurrentSystemArch = lift getCurrentSystemArch

instance MonadEnv IO where
    getEnvVar = lookupEnv

    getCurrentSystemOS = return $ T.pack System.Info.os

    -- Invert the conversion done by GHC_CONVERT_CPU in GHC's aclocal.m4
    getCurrentSystemArch = return $ T.pack $ case System.Info.arch of
      "i386" -> "i686"
      arch -> arch

class Monad m => MonadHttp m where
    getURL :: Text -> m (Either ErrorCall StorePath)
    default getURL :: (MonadTrans t, MonadHttp m', m ~ t m') => Text -> m (Either ErrorCall StorePath)
    getURL = lift . getURL

instance MonadHttp IO where
    getURL url = do
        let urlstr = T.unpack url
        traceM $ "fetching HTTP URL: " ++ urlstr
        req <- parseRequest urlstr
        manager <-
          if secure req
          then newTlsManager
          else newManager defaultManagerSettings
        -- print req
        response <- httpLbs (req { method = "GET" }) manager
        let status = statusCode (responseStatus response)
        if  status /= 200
          then return $ Left $ ErrorCall $
                 "fail, got " ++ show status ++ " when fetching url:" ++ urlstr
          else -- do
            -- let bstr = responseBody response
            return $ Left $ ErrorCall $
              "success in downloading but hnix-store is not yet ready; url = " ++ urlstr


class Monad m => MonadPutStr m where
    --TODO: Should this be used *only* when the Nix to be evaluated invokes a
    --`trace` operation?
    putStr :: String -> m ()
    default putStr :: (MonadTrans t, MonadPutStr m', m ~ t m') => String -> m ()
    putStr = lift . putStr

putStrLn :: MonadPutStr m => String -> m ()
putStrLn = putStr . (++"\n")

print :: (MonadPutStr m, Show a) => a -> m ()
print = putStrLn . show

instance MonadPutStr IO where
    putStr = Prelude.putStr

class Monad m => MonadStore m where
    -- | Import a path into the nix store, and return the resulting path
    addPath' :: FilePath -> m (Either ErrorCall StorePath)

    -- | Add a file with the given name and contents to the nix store
    toFile_' :: FilePath -> String -> m (Either ErrorCall StorePath)

instance MonadStore IO where
    addPath' path = do
        (exitCode, out, _) <-
            readProcessWithExitCode "nix-store" ["--add", path] ""
        case exitCode of
          ExitSuccess -> do
            let dropTrailingLinefeed p = take (length p - 1) p
            return $ Right $ StorePath $ dropTrailingLinefeed out
          _ -> return $ Left $ ErrorCall $
                  "addPath: failed: nix-store --add " ++ show path

    --TODO: Use a temp directory so we don't overwrite anything important
    toFile_' filepath content = do
      writeFile filepath content
      storepath <- addPath' filepath
      S.removeFile filepath
      return storepath

addPath :: (Framed e m, MonadStore m) => FilePath -> m StorePath
addPath p = either throwError return =<< addPath' p

-- TODO Now that we're getting to proper implementations of store functions, we
-- might be able to remove this.
toFile_ :: (Framed e m, MonadStore m) => FilePath -> String -> m StorePath
toFile_ p contents = either throwError return =<< toFile_' p contents
