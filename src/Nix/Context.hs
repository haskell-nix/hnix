module Nix.Context where

import           Nix.Prelude
import           Nix.Options                    ( Options )
import           Nix.Scope                      ( Scopes )
import           Nix.Frames                     ( Frames )
import           Nix.EvalStats                  ( EvalStats )
import           Nix.Expr.Types.Annotated       ( SrcSpan
                                                , nullSpan
                                                )

--  2021-07-18: NOTE: It should be Options -> Scopes -> Frames -> Source(span)
data Context m t =
  Context
    { getOptions   :: !Options
    , getScopes    :: !(Scopes m t)
    , getSource    :: !SrcSpan
    , getFrames    :: !Frames
    , getEvalStats :: !(Maybe EvalStats)
    }

instance Has (Context m t) (Scopes m t) where
  hasLens f a = (\x -> a { getScopes = x }) <$> f (getScopes a)

instance Has (Context m t) SrcSpan where
  hasLens f a = (\x -> a { getSource = x }) <$> f (getSource a)

instance Has (Context m t) Frames where
  hasLens f a = (\x -> a { getFrames = x }) <$> f (getFrames a)

instance Has (Context m t) Options where
  hasLens f a = (\x -> a { getOptions = x }) <$> f (getOptions a)

instance Has (Context m t) (Maybe EvalStats) where
  hasLens f a = (\x -> a { getEvalStats = x }) <$> f (getEvalStats a)

newContext :: Options -> Context m t
newContext o = Context o mempty nullSpan mempty Nothing

newContextWithStats :: Options -> Maybe EvalStats -> Context m t
newContextWithStats o stats = Context o mempty nullSpan mempty stats

askEvalStats :: forall e m . (MonadReader e m, Has e (Maybe EvalStats)) => m (Maybe EvalStats)
askEvalStats = askLocal
