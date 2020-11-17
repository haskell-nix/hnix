{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Nix.Effects (
    module Nix.Effects
  , Store.PathType (..)
  ) where

import           Prelude                 hiding ( putStr
                                                , putStrLn
                                                , print
                                                )
import qualified Prelude

import           Control.Monad.Trans
import qualified Data.HashSet                  as HS
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as T
import           Network.HTTP.Client     hiding ( path, Proxy )
import           Network.HTTP.Client.TLS
import           Network.HTTP.Types
import           Nix.Expr
import           Nix.Frames              hiding ( Proxy )
import           Nix.Parser
import           Nix.Render
import           Nix.Utils
import           Nix.Value
import qualified Paths_hnix
import           System.Environment
import           System.Exit
import           System.FilePath                ( takeFileName )
import qualified System.Info
import           System.Process

import qualified System.Nix.Hash               as Store
import qualified System.Nix.Store.Remote       as Store
import qualified System.Nix.Store.Remote.Types as Store
import qualified System.Nix.StorePath          as Store

-- | A path into the nix store
newtype StorePath = StorePath { unStorePath :: FilePath }

class (MonadFile m,
       MonadStore m,
       MonadPutStr m,
       MonadHttp m,
       MonadEnv m,
       MonadPaths m,
       MonadInstantiate m,
       MonadExec m,
       MonadIntrospect m) => MonadEffects t f m where
  -- | Determine the absolute path of relative path in the current context
  makeAbsolutePath :: FilePath -> m FilePath
  findEnvPath :: String -> m FilePath

  -- | Having an explicit list of sets corresponding to the NIX_PATH
  -- and a file path try to find an existing path
  findPath :: [NValue t f m] -> FilePath -> m FilePath

  importPath :: FilePath -> m (NValue t f m)
  pathToDefaultNix :: FilePath -> m FilePath

  derivationStrict :: NValue t f m -> m (NValue t f m)

  traceEffect :: String -> m ()

class Monad m => MonadIntrospect m where
  recursiveSize :: a -> m Word
  default recursiveSize :: (MonadTrans t, MonadIntrospect m', m ~ t m') => a -> m Word
  recursiveSize = lift . recursiveSize

instance MonadIntrospect IO where
  recursiveSize =
#ifdef MIN_VERSION_ghc_datasize
#if MIN_VERSION_ghc_datasize(0,2,0)
recursiveSize
#else
\_ -> return 0
#endif
#else
    \_ -> pure 0
#endif

class Monad m => MonadExec m where
    exec' :: [String] -> m (Either ErrorCall NExprLoc)
    default exec' :: (MonadTrans t, MonadExec m', m ~ t m')
                  => [String] -> m (Either ErrorCall NExprLoc)
    exec' = lift . exec'

instance MonadExec IO where
  exec' = \case
    []            -> pure $ Left $ ErrorCall "exec: missing program"
    (prog : args) -> do
      (exitCode, out, _) <- liftIO $ readProcessWithExitCode prog args ""
      let t    = T.strip (T.pack out)
      let emsg = "program[" ++ prog ++ "] args=" ++ show args
      case exitCode of
        ExitSuccess -> if T.null t
          then pure $ Left $ ErrorCall $ "exec has no output :" ++ emsg
          else case parseNixTextLoc t of
            Failure err ->
              pure
                $  Left
                $  ErrorCall
                $  "Error parsing output of exec: "
                ++ show err
                ++ " "
                ++ emsg
            Success v -> pure $ Right v
        err ->
          pure
            $  Left
            $  ErrorCall
            $  "exec  failed: "
            ++ show err
            ++ " "
            ++ emsg

class Monad m => MonadInstantiate m where
    instantiateExpr :: String -> m (Either ErrorCall NExprLoc)
    default instantiateExpr :: (MonadTrans t, MonadInstantiate m', m ~ t m') => String -> m (Either ErrorCall NExprLoc)
    instantiateExpr = lift . instantiateExpr

instance MonadInstantiate IO where
  instantiateExpr expr = do
    traceM $ "Executing: " ++ show
      ["nix-instantiate", "--eval", "--expr ", expr]
    (exitCode, out, err) <- readProcessWithExitCode "nix-instantiate"
                                                    ["--eval", "--expr", expr]
                                                    ""
    case exitCode of
      ExitSuccess -> case parseNixTextLoc (T.pack out) of
        Failure e ->
          pure
            $  Left
            $  ErrorCall
            $  "Error parsing output of nix-instantiate: "
            ++ show e
        Success v -> pure $ Right v
      status ->
        pure
          $  Left
          $  ErrorCall
          $  "nix-instantiate failed: "
          ++ show status
          ++ ": "
          ++ err

pathExists :: MonadFile m => FilePath -> m Bool
pathExists = doesPathExist

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
  getEnvVar            = lookupEnv

  getCurrentSystemOS   = pure $ T.pack System.Info.os

  -- Invert the conversion done by GHC_CONVERT_CPU in GHC's aclocal.m4
  getCurrentSystemArch = pure $ T.pack $ case System.Info.arch of
    "i386" -> "i686"
    arch   -> arch

class Monad m => MonadPaths m where
    getDataDir :: m FilePath
    default getDataDir :: (MonadTrans t, MonadPaths m', m ~ t m') => m FilePath
    getDataDir = lift getDataDir

instance MonadPaths IO where
    getDataDir = Paths_hnix.getDataDir

class Monad m => MonadHttp m where
    getURL :: Text -> m (Either ErrorCall StorePath)
    default getURL :: (MonadTrans t, MonadHttp m', m ~ t m') => Text -> m (Either ErrorCall StorePath)
    getURL = lift . getURL

instance MonadHttp IO where
  getURL url = do
    let urlstr = T.unpack url
    traceM $ "fetching HTTP URL: " ++ urlstr
    req     <- parseRequest urlstr
    manager <- if secure req
      then newTlsManager
      else newManager defaultManagerSettings
    -- print req
    response <- httpLbs (req { method = "GET" }) manager
    let status = statusCode (responseStatus response)
    if status /= 200
      then
        pure
        $  Left
        $  ErrorCall
        $  "fail, got "
        ++ show status
        ++ " when fetching url:"
        ++ urlstr
      else -- do
        -- let bstr = responseBody response
        pure
        $  Left
        $  ErrorCall
        $  "success in downloading but hnix-store is not yet ready; url = "
        ++ urlstr


class Monad m => MonadPutStr m where
    --TODO: Should this be used *only* when the Nix to be evaluated invokes a
    --`trace` operation?
    putStr :: String -> m ()
    default putStr :: (MonadTrans t, MonadPutStr m', m ~ t m') => String -> m ()
    putStr = lift . putStr

putStrLn :: MonadPutStr m => String -> m ()
putStrLn = putStr . (++ "\n")

print :: (MonadPutStr m, Show a) => a -> m ()
print = putStrLn . show

instance MonadPutStr IO where
  putStr = Prelude.putStr


type RecursiveFlag = Bool
type RepairFlag = Bool
type StorePathName = Text
type FilePathFilter m = FilePath -> Store.PathType -> m Bool
type StorePathSet = HS.HashSet StorePath

class (MonadIO m, Store.MonadRemoteStore m) => MonadStore m where

  -- | Add a path to the store, with bells and whistles
  addToStore :: StorePathName -> FilePath -> FilePathFilter m -> RecursiveFlag -> RepairFlag -> m StorePath
  default addToStore :: StorePathName -> FilePath -> FilePathFilter m -> RecursiveFlag -> RepairFlag -> m StorePath
  addToStore name path filter recursive repair = do
    -- TODO: replace this error call by something smarter. throwE ? throwError ?
    pathName <- either error return $ Store.makeStorePathName name
    convertStorePath <$> Store.addToStore @'Store.SHA256 pathName path recursive filter repair

  addTextToStore :: StorePathName -> Text -> Store.StorePathSet -> RepairFlag -> m StorePath
  default addTextToStore :: StorePathName -> Text -> Store.StorePathSet -> RepairFlag -> m StorePath
  addTextToStore name text references repair =
    convertStorePath <$> Store.addTextToStore name text references repair


-- XXX (layus) relying on show is not ideal, but way more concise.
-- Bound to disappear anyway if we unify StorePath representation across hnix* projects
convertStorePath :: Store.StorePath -> StorePath
convertStorePath = StorePath . show

toFile_ :: MonadStore m => FilePath -> String -> m StorePath
toFile_ p contents = addTextToStore (T.pack p) (T.pack contents) HS.empty False

addPath :: (MonadStore m) => FilePath -> m StorePath
addPath p = addToStore (T.pack $ takeFileName p) p (\_p _pt -> pure True) True False
