{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Nix.Context where

import Nix.Options
import Nix.Scope
import Nix.Frames
import Nix.Utils
import Nix.Expr.Types.Annotated (SrcSpan, nullSpan)

data Context m v = Context
    { scopes  :: Scopes m v
    , source  :: SrcSpan
    , frames  :: Frames
    , options :: Options
    }

instance Has (Context m v) (Scopes m v) where
    hasLens f (Context x y z w) = (\x' -> Context x' y z w) <$> f x

instance Has (Context m v) SrcSpan where
    hasLens f (Context x y z w) = (\y' -> Context x y' z w) <$> f y

instance Has (Context m v) Frames where
    hasLens f (Context x y z w) = (\z' -> Context x y z' w) <$> f z

instance Has (Context m v) Options where
    hasLens f (Context x y z w) = (\w' -> Context x y z w') <$> f w

newContext :: Options -> Context m v
newContext = Context emptyScopes nullSpan []
