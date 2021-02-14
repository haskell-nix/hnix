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

data Context m t = Context
    { scopes  :: Scopes m t
    , source  :: SrcSpan
    , frames  :: Frames
    , options :: Options
    }

instance Has (Context m t) (Scopes m t) where
  hasLens f a = (\x -> a { scopes = x }) <$> f (scopes a)

instance Has (Context m t) SrcSpan where
  hasLens f a = (\x -> a { source = x }) <$> f (source a)

instance Has (Context m t) Frames where
  hasLens f a = (\x -> a { frames = x }) <$> f (frames a)

instance Has (Context m t) Options where
  hasLens f a = (\x -> a { options = x }) <$> f (options a)

newContext :: Options -> Context m t
newContext = Context emptyScopes nullSpan mempty
