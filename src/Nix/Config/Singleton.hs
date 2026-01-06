{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Type-level configuration for HNix evaluation.
--
-- This module provides compile-time specialization for evaluation hot paths
-- using the @singleton-bool@ library. Configuration flags (stats collection,
-- provenance tracking, tracing) are lifted to the type level, allowing GHC
-- to generate specialized code with zero runtime overhead for disabled features.
--
-- == Usage
--
-- At program startup, use 'withEvalConfig' to bridge runtime options to
-- type-level configuration:
--
-- @
-- main :: IO ()
-- main = do
--   opts <- parseOptions
--   withEvalConfig
--     (isEvalStats opts)
--     (isProvenance opts)
--     (isTrace opts)
--     $ \(_ :: Proxy '(s, p, t)) ->
--         runEvaluation @s @p @t ...
-- @
--
-- Within evaluation code, use 'whenStats', 'whenProvenance', and 'whenTracing'
-- for zero-cost conditional execution:
--
-- @
-- evalExpr :: forall s p t m. EvalConfig s p t => Expr -> m Value
-- evalExpr expr = do
--   whenStats @s $ recordExprStart expr
--   result <- doEval expr
--   whenStats @s $ recordExprEnd expr
--   pure result
-- @
--
-- When @s ~ 'False@, GHC eliminates the 'whenStats' calls entirely via
-- case-of-known-constructor optimization.
module Nix.Config.Singleton
  ( -- * Type-level configuration flags
    EvalConfig
  , HasStats
  , HasProvenance
  , HasTracing
    -- * Conditional operations (zero-cost when disabled)
  , whenStats
  , whenProvenance
  , whenTracing
  , ifStats
  , ifProvenance
  , ifTracing
    -- * Monadic conditional operations
  , ifStatsM
  , ifProvenanceM
  , ifTracingM
    -- * Runtime bridge
  , withEvalConfig
  , SomeEvalConfig(..)
    -- * Re-exports from singleton-bool
  , SBoolI(..)
  ) where

import           Relude

import           Data.Singletons.Bool (SBool(..), SBoolI(..), reifyBool)

-- | Constraint bundle for evaluation configuration.
--
-- This constraint provides access to all three configuration flags at the
-- type level. Functions with this constraint can use 'whenStats',
-- 'whenProvenance', and 'whenTracing' for zero-cost conditional execution.
type EvalConfig (stats :: Bool) (prov :: Bool) (trace :: Bool) =
  (HasStats stats, HasProvenance prov, HasTracing trace)

-- | Individual constraint for stats collection flag.
type HasStats s = SBoolI s

-- | Individual constraint for provenance tracking flag.
type HasProvenance p = SBoolI p

-- | Individual constraint for tracing flag.
type HasTracing t = SBoolI t

-- | Execute action only when stats collection is enabled at the type level.
--
-- When @s ~ 'False@, GHC eliminates this call entirely at compile time.
--
-- @
-- whenStats @s $ recordStat "function_application"
-- @
whenStats :: forall s m. (HasStats s, Applicative m) => m () -> m ()
whenStats action = case sbool @s of
  STrue  -> action
  SFalse -> pure ()
{-# INLINABLE whenStats #-}

-- | Execute action only when provenance tracking is enabled at the type level.
--
-- When @p ~ 'False@, GHC eliminates this call entirely at compile time.
whenProvenance :: forall p m. (HasProvenance p, Applicative m) => m () -> m ()
whenProvenance action = case sbool @p of
  STrue  -> action
  SFalse -> pure ()
{-# INLINABLE whenProvenance #-}

-- | Execute action only when tracing is enabled at the type level.
--
-- When @t ~ 'False@, GHC eliminates this call entirely at compile time.
whenTracing :: forall t m. (HasTracing t, Applicative m) => m () -> m ()
whenTracing action = case sbool @t of
  STrue  -> action
  SFalse -> pure ()
{-# INLINABLE whenTracing #-}

-- | Choose between two values based on whether stats collection is enabled.
--
-- When @s@ is statically known, GHC eliminates the branch entirely.
--
-- @
-- result <- ifStats @s
--   (evalWithStats expr)
--   (evalWithoutStats expr)
-- @
ifStats :: forall s a. HasStats s => a -> a -> a
ifStats whenTrue whenFalse = case sbool @s of
  STrue  -> whenTrue
  SFalse -> whenFalse
{-# INLINABLE ifStats #-}

-- | Choose between two values based on whether provenance tracking is enabled.
ifProvenance :: forall p a. HasProvenance p => a -> a -> a
ifProvenance whenTrue whenFalse = case sbool @p of
  STrue  -> whenTrue
  SFalse -> whenFalse
{-# INLINABLE ifProvenance #-}

-- | Choose between two values based on whether tracing is enabled.
ifTracing :: forall t a. HasTracing t => a -> a -> a
ifTracing whenTrue whenFalse = case sbool @t of
  STrue  -> whenTrue
  SFalse -> whenFalse
{-# INLINABLE ifTracing #-}

-- | Monadic version of 'ifStats' - choose between two actions.
--
-- When @s@ is statically known, GHC eliminates the branch entirely.
ifStatsM :: forall s m a. (HasStats s, Monad m) => m a -> m a -> m a
ifStatsM whenTrue whenFalse = case sbool @s of
  STrue  -> whenTrue
  SFalse -> whenFalse
{-# INLINABLE ifStatsM #-}

-- | Monadic version of 'ifProvenance' - choose between two actions.
ifProvenanceM :: forall p m a. (HasProvenance p, Monad m) => m a -> m a -> m a
ifProvenanceM whenTrue whenFalse = case sbool @p of
  STrue  -> whenTrue
  SFalse -> whenFalse
{-# INLINABLE ifProvenanceM #-}

-- | Monadic version of 'ifTracing' - choose between two actions.
ifTracingM :: forall t m a. (HasTracing t, Monad m) => m a -> m a -> m a
ifTracingM whenTrue whenFalse = case sbool @t of
  STrue  -> whenTrue
  SFalse -> whenFalse
{-# INLINABLE ifTracingM #-}

-- | Existentially wrapped configuration for runtime dispatch.
--
-- This type allows storing a configuration that was determined at runtime
-- and using it later with pattern matching.
data SomeEvalConfig where
  MkEvalConfig :: forall s p t. EvalConfig s p t
               => Proxy '(s, p, t) -> SomeEvalConfig

-- | Bridge runtime booleans to type-level configuration.
--
-- This function should be called ONCE at program startup to convert
-- runtime configuration options into type-level parameters. All subsequent
-- evaluation uses the type-level parameters, enabling compile-time
-- specialization.
--
-- @
-- main = do
--   opts <- parseOptions
--   withEvalConfig
--     (collectStats opts)
--     (trackProvenance opts)
--     (enableTracing opts)
--     $ \(_ :: Proxy '(s, p, t)) -> do
--         -- From here, all evaluation uses @s, @p, @t type applications
--         runEvaluation @s @p @t ...
-- @
--
-- The CPS style ensures that the type-level configuration is available
-- throughout the entire evaluation scope.
withEvalConfig
  :: Bool  -- ^ Collect stats
  -> Bool  -- ^ Track provenance
  -> Bool  -- ^ Enable tracing
  -> (forall s p t. EvalConfig s p t => Proxy '(s, p, t) -> r)
  -> r
withEvalConfig stats prov tracing k =
  reifyBool stats $ \(_ :: Proxy s) ->
    reifyBool prov $ \(_ :: Proxy p) ->
      reifyBool tracing $ \(_ :: Proxy t) ->
        k (Proxy @'(s, p, t))
{-# INLINABLE withEvalConfig #-}
