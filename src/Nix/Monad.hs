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
import Nix.Value
import System.Posix.Files

-- | A path into the nix store
newtype StorePath = StorePath { unStorePath :: FilePath }

class Monad m => MonadNix m where
    -- | Import a path into the nix store, and return the resulting path
    addPath :: FilePath -> m StorePath

    -- | Determine the absolute path of relative path in the current context
    makeAbsolutePath :: FilePath -> m FilePath

    pathExists :: FilePath -> m Bool
    importFile :: ValueSet m -> FilePath -> m (NValue m)
    getEnvVar :: String -> m (Maybe String)
    getCurrentSystemOS :: m Text
    getCurrentSystemArch :: m Text

    listDirectory :: FilePath -> m [FilePath]
    getSymbolicLinkStatus :: FilePath -> m FileStatus

builtin :: MonadNix m => String -> (NThunk m -> m (NValue m)) -> m (NValue m)
builtin name f = return $ NVBuiltin name f

builtin2 :: MonadNix m
         => String -> (NThunk m -> NThunk m -> m (NValue m)) -> m (NValue m)
builtin2 name f = builtin name (builtin name . f)

builtin3 :: MonadNix m
         => String -> (NThunk m -> NThunk m -> NThunk m -> m (NValue m))
         -> m (NValue m)
builtin3 name f =
    builtin name $ \a -> builtin name $ \b -> builtin name $ \c -> f a b c
