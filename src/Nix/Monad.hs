{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Nix.Monad where

import Data.Text (Text)
import Data.HashMap.Strict (HashMap)
import Nix.Value
import System.Directory
import System.FilePath.Posix
import System.Posix.Files

-- | A path into the nix store
newtype StorePath = StorePath { unStorePath :: FilePath }

class Monad m => MonadNix m where
    -- | Import a path into the nix store, and return the resulting path
    addPath :: FilePath -> m StorePath

    -- | Determine the absolute path of relative path in the current context
    makeAbsolutePath :: FilePath -> m FilePath
    findEnvPath :: String -> m FilePath

    pathExists :: FilePath -> m Bool
    importPath :: ValueSet m -> FilePath -> m (NValue m)

    getEnvVar :: String -> m (Maybe String)
    getCurrentSystemOS :: m Text
    getCurrentSystemArch :: m Text

    listDirectory :: FilePath -> m [FilePath]
    getSymbolicLinkStatus :: FilePath -> m FileStatus

    derivationStrict :: NValueNF m -> m (HashMap Text Text)

-- Given a path, determine the nix file to load
pathToDefaultNixFile :: FilePath -> IO FilePath
pathToDefaultNixFile p = do
    isDir <- doesDirectoryExist p
    pure $ if isDir
        then p </> "default.nix"
        else p
