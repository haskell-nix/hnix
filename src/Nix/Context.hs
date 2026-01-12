{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Nix.Context
  ( Context(..)
  , CtxCfg
  , HasEvalCfg
    -- * Per-flag constraint aliases (shorter than 'HasProvCfg (CtxCfg e)')
  , HasStatsCfgE
  , HasProvCfgE
  , HasTraceCfgE
  , newContext
  , newContextWithStats
  , askEvalStats
  ) where

import           Nix.Prelude
import           GHC.TypeLits                   ( TypeError, ErrorMessage(..) )
import           Nix.Config.Singleton           ( EvalCfg, KnownEvalCfg
                                                , HasStatsCfg, HasProvCfg, HasTraceCfg
                                                )
import           Nix.Options                    ( Options )
import           Nix.Scope                      ( Scopes )
import           Nix.Frames                     ( Frames )
import           Nix.EvalStats                  ( EvalStats )
import           Nix.Expr.Types.Annotated       ( SrcSpan
                                                , nullSpan
                                                )

-- | Type family to extract configuration from Context.
-- This enables compile-time dispatch in MonadEval instances.
--
-- If you're wrapping Context in a newtype, you'll need to add a type
-- instance for your wrapper. For example:
--
-- @
-- newtype MyEnv cfg m t = MyEnv (Context cfg m t)
-- type instance CtxCfg (MyEnv cfg m t) = cfg
-- @
type family CtxCfg e where
  CtxCfg (Context cfg m t) = cfg
  CtxCfg e = TypeError
    ( 'Text "Cannot extract EvalCfg from type: " ':<>: 'ShowType e
    ':$$: 'Text "CtxCfg only works with 'Context cfg m t' or types with a CtxCfg instance."
    ':$$: 'Text "If using a custom environment wrapper, add: type instance CtxCfg (YourType ...) = cfg"
    )

-- | Constraint that the environment's configuration is known at compile time.
-- Use this instead of passing an independent @cfg@ type parameter - it ties
-- the configuration to the environment and guarantees consistency across
-- stats, provenance, and tracing dispatch.
--
-- Example:
--
-- @
-- evalExprLoc :: (MonadNix e t f m, HasEvalCfg e) => NExprLoc -> m (NValue t f m)
-- evalExprLoc = case singTrace @(CtxCfg e) of ...
-- @
type HasEvalCfg e = KnownEvalCfg (CtxCfg e)

-- | Constraint for functions that only need stats flag from environment.
-- Use this instead of @HasStatsCfg (CtxCfg e)@ to minimize verbosity.
type HasStatsCfgE e = HasStatsCfg (CtxCfg e)

-- | Constraint for functions that only need provenance flag from environment.
-- Use this instead of @HasProvCfg (CtxCfg e)@ to minimize verbosity.
type HasProvCfgE e = HasProvCfg (CtxCfg e)

-- | Constraint for functions that only need trace flag from environment.
-- Use this instead of @HasTraceCfg (CtxCfg e)@ to minimize verbosity.
type HasTraceCfgE e = HasTraceCfg (CtxCfg e)

--  2021-07-18: NOTE: It should be Options -> Scopes -> Frames -> Source(span)
-- | Evaluation context parameterized by compile-time configuration.
--
-- The @cfg@ parameter is a phantom type that carries the 'EvalCfg'
-- configuration. This enables zero-cost conditional execution in
-- the 'MonadEval' instance - GHC eliminates unused branches when
-- @cfg@ is known at compile time.
data Context (cfg :: EvalCfg) m t =
  Context
    { getOptions   :: !Options
    , getScopes    :: !(Scopes m t)
    , getSource    :: !SrcSpan
    , getFrames    :: !Frames
    , getEvalStats :: !(Maybe EvalStats)
    }

instance Has (Context cfg m t) (Scopes m t) where
  hasLens f a = (\x -> a { getScopes = x }) <$> f (getScopes a)

instance Has (Context cfg m t) SrcSpan where
  hasLens f a = (\x -> a { getSource = x }) <$> f (getSource a)

instance Has (Context cfg m t) Frames where
  hasLens f a = (\x -> a { getFrames = x }) <$> f (getFrames a)

instance Has (Context cfg m t) Options where
  hasLens f a = (\x -> a { getOptions = x }) <$> f (getOptions a)

instance Has (Context cfg m t) (Maybe EvalStats) where
  hasLens f a = (\x -> a { getEvalStats = x }) <$> f (getEvalStats a)

newContext :: Options -> Context cfg m t
newContext o = Context o mempty nullSpan mempty Nothing

newContextWithStats :: Options -> Maybe EvalStats -> Context cfg m t
newContextWithStats o stats = Context o mempty nullSpan mempty stats

askEvalStats :: forall e m . (MonadReader e m, Has e (Maybe EvalStats)) => m (Maybe EvalStats)
askEvalStats = askLocal
