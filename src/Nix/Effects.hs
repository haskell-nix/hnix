{-# LANGUAGE FlexibleContexts #-}
module Nix.Effects where

import Data.Text (Text)
import Nix.Frames
import Nix.Render
import Nix.Value
import System.Exit
import System.Posix.Files
import System.Process
import System.Directory

-- | A path into the nix store
newtype StorePath = StorePath { unStorePath :: FilePath }

class (MonadFile m, MonadStore m) => MonadEffects m where
    -- | Determine the absolute path of relative path in the current context
    makeAbsolutePath :: FilePath -> m FilePath
    findEnvPath :: String -> m FilePath

    -- | Having an explicit list of sets corresponding to the NIX_PATH
    -- and a file path try to find an existing path
    findPath :: [NThunk m] -> FilePath -> m FilePath

    pathExists :: FilePath -> m Bool
    importPath :: FilePath -> m (NValue m)
    pathToDefaultNix :: FilePath -> m FilePath

    getEnvVar :: String -> m (Maybe String)
    getCurrentSystemOS :: m Text
    getCurrentSystemArch :: m Text

    listDirectory :: FilePath -> m [FilePath]
    getSymbolicLinkStatus :: FilePath -> m FileStatus

    derivationStrict :: NValue m -> m (NValue m)

    nixInstantiateExpr :: String -> m (NValue m)

    getURL :: Text -> m (NValue m)

    getRecursiveSize :: a -> m (NValue m)

    traceEffect :: String -> m ()

    exec :: [String] -> m (NValue m)

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
      removeFile filepath
      return storepath

addPath :: (Framed e m, MonadStore m) => FilePath -> m StorePath
addPath p = either throwError return =<< addPath' p

toFile_ :: (Framed e m, MonadStore m) => FilePath -> String -> m StorePath
toFile_ p contents = either throwError return =<< toFile_' p contents
