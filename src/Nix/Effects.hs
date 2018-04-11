module Nix.Effects where

import Data.Text (Text)
import System.Posix.Files
import Nix.Value
import Nix.Utils

-- | A path into the nix store
newtype StorePath = StorePath { unStorePath :: FilePath }

class MonadEffects m where
    -- | Import a path into the nix store, and return the resulting path
    addPath :: FilePath -> m StorePath

    -- | Determine the absolute path of relative path in the current context
    makeAbsolutePath :: FilePath -> m FilePath
    findEnvPath :: String -> m FilePath

    pathExists :: FilePath -> m Bool
    importPath :: AttrSet (NThunk m) -> FilePath -> m (NValue m)

    getEnvVar :: String -> m (Maybe String)
    getCurrentSystemOS :: m Text
    getCurrentSystemArch :: m Text

    listDirectory :: FilePath -> m [FilePath]
    getSymbolicLinkStatus :: FilePath -> m FileStatus

    derivationStrict :: NValue m -> m (NValue m)

    nixInstantiateExpr :: String -> m (NValue m)
