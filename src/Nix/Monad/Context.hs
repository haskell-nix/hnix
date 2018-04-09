{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Nix.Monad.Context where

import Nix.Scope
import Nix.Stack
import Nix.Utils

data Context m v = Context
    { scopes :: Scopes m v
    , frames :: Frames
    }

instance Has (Context m v) (Scopes m v) where
    hasLens f (Context x y) = flip Context y <$> f x

instance Has (Context m v) Frames where
    hasLens f (Context x y) = Context x <$> f y
