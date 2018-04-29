{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Nix.Context where

import Data.Time.Clock.POSIX (POSIXTime)
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
    , currentTime :: POSIXTime
    }

instance Has (Context m v) (Scopes m v) where
    hasLens f (Context x y z w t) = (\x' -> Context x' y z w t) <$> f x

instance Has (Context m v) SrcSpan where
    hasLens f (Context x y z w t) = (\y' -> Context x y' z w t) <$> f y

instance Has (Context m v) Frames where
    hasLens f (Context x y z w t) = (\z' -> Context x y z' w t) <$> f z

instance Has (Context m v) Options where
    hasLens f (Context x y z w t) = (\w' -> Context x y z w' t) <$> f w

instance Has (Context m v) POSIXTime where
    hasLens f (Context x y z w t) = (\t' -> Context x y z w t') <$> f t

newContext :: Options -> POSIXTime -> Context m v
newContext = Context emptyScopes nullSpan []
