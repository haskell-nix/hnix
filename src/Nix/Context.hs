{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Nix.Context where

import Nix.Scope
import Nix.Stack
import Nix.Utils

data Context m v = Context
    { scopes     :: Scopes m v
    , frames     :: Frames
    }

instance Has (Context m v) (Scopes m v) where
    hasLens f (Context x y) = (\x' -> Context x' y) <$> f x

instance Has (Context m v) Frames where
    hasLens f (Context x y) = (\y' -> Context x y') <$> f y

newContext :: Context m v
newContext = Context emptyScopes []
