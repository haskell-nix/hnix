{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Nix.Context where

import           Nix.Options
import           Nix.Scope
import           Nix.Frames
import           Nix.Utils
import           Nix.Expr.Types.Annotated       ( SrcSpan
                                                , nullSpan
                                                )

data Context = Context
    { source  :: SrcSpan -- Should we capture?
    , frames  :: Frames -- Don't capture (should change)
    , options :: Options -- Don't capture (never changes)
    }

instance Has Context SrcSpan where
  hasLens f (Context x y z) = (\x' -> Context x' y z) <$> f x

instance Has Context Frames where
  hasLens f (Context x y z) = (\y' -> Context x y' z) <$> f y

instance Has Context Options where
  hasLens f (Context x y z) = (\z' -> Context x y z') <$> f z

newContext :: Options -> Context
newContext = Context nullSpan []
