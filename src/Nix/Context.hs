module Nix.Context where

import           Nix.Prelude
import           Nix.Options                    ( Options )
import           Nix.Scope                      ( Scopes )
import           Nix.Frames                     ( Frames )
import           Nix.Expr.Types.Annotated       ( SrcSpan
                                                , nullSpan
                                                )

--  2021-07-18: NOTE: It should be Options -> Scopes -> Frames -> Source(span)
data Context m t =
  Context
    { getOptions :: Options
    , getScopes  :: Scopes m t
    , getSource  :: SrcSpan
    , getFrames  :: Frames
    }

instance Has (Context m t) (Scopes m t) where
  hasLens f a = (\x -> a { getScopes = x }) <$> f (getScopes a)

instance Has (Context m t) SrcSpan where
  hasLens f a = (\x -> a { getSource = x }) <$> f (getSource a)

instance Has (Context m t) Frames where
  hasLens f a = (\x -> a { getFrames = x }) <$> f (getFrames a)

instance Has (Context m t) Options where
  hasLens f a = (\x -> a { getOptions = x }) <$> f (getOptions a)

newContext :: Options -> Context m t
newContext o = Context o mempty nullSpan mempty
