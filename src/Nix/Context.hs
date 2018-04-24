{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Nix.Context where

import Nix.Options
import Nix.Scope
import Nix.Frames
import Nix.Utils

data Context m v = Context
    { scopes  :: Scopes m v
    , frames  :: Frames
    , options :: Options
    }

instance Has (Context m v) (Scopes m v) where
    hasLens f (Context x y z) = (\x' -> Context x' y z) <$> f x

instance Has (Context m v) Frames where
    hasLens f (Context x y z) = (\y' -> Context x y' z) <$> f y

instance Has (Context m v) Options where
    hasLens f (Context x y z) = (\z' -> Context x y z') <$> f z

newContext :: Options -> Context m v
newContext = Context emptyScopes []
