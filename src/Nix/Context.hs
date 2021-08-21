
module Nix.Context where

import           Nix.Options                    ( Options )
import           Nix.Scope                      ( Scopes )
import           Nix.Frames                     ( Frames )
import           Nix.Expr.Types.Annotated       ( SrcSpan
                                                , nullSpan
                                                )

--  2021-07-18: NOTE: It should be Options -> Scopes -> Frames -> Source(span)
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
newContext = Context mempty nullSpan mempty
