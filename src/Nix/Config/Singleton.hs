{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | Type-level evaluation configuration for zero-cost conditional execution.
--
-- This module provides compile-time specialization for evaluation hot paths
-- using a type-level configuration record. Configuration flags (stats collection,
-- provenance tracking, tracing) are bundled into a single 'EvalCfg' type,
-- allowing GHC to generate specialized code with zero runtime overhead for
-- disabled features.
--
-- == Usage
--
-- At program startup, use 'withEvalCfg' to bridge runtime options to
-- type-level configuration:
--
-- @
-- main :: IO ()
-- main = do
--   opts <- parseOptions
--   withEvalCfg
--     (isEvalStats opts)
--     (isValues opts)
--     (isTrace opts)
--     $ \\(_ :: Proxy cfg) ->
--         runEvaluation \@cfg ...
-- @
--
-- Within evaluation code, use 'singStats', 'singProv', and 'singTrace'
-- for zero-cost conditional execution:
--
-- @
-- evalExpr :: forall cfg m. KnownEvalCfg cfg => Expr -> m Value
-- evalExpr expr = case singProv \@cfg of
--   STrue  -> evalWithProvenance expr
--   SFalse -> evalWithoutProvenance expr
-- @
--
-- When @CfgProv cfg ~ 'False@, GHC eliminates the 'STrue' branch entirely via
-- case-of-known-constructor optimization.
module Nix.Config.Singleton
  ( -- * Configuration type
    EvalCfg  -- Abstract: constructor not exported
  , KnownEvalCfg
    -- * Default configuration (all flags disabled, for tests)
  , DefaultCfg
    -- * Per-flag constraints (use these for minimal constraints)
  , HasStatsCfg
  , HasProvCfg
  , HasTraceCfg
    -- * Type families
  , CfgStats
  , CfgProv
  , CfgTrace
    -- * Singleton accessors (zero-cost)
  , singStats
  , singProv
  , singTrace
    -- * Helper functions (zero-cost dispatch)
  , ifSBool
  , ifStats
  , ifProv
  , ifTrace
  , whenStatsM
  , whenProvM
  , whenTraceM
    -- * Runtime bridge
  , withEvalCfg
    -- * Re-exports from singleton-bool
  , SBool(..)
  , SBoolI(..)
  ) where

import           Relude

import           Data.Singletons.Bool (SBool(..), SBoolI(..), sbool)

-- | Like 'reifyBool' from singleton-bool, but also provides 'Typeable' constraint.
-- This is needed because the standard 'reifyBool' only provides 'SBoolI',
-- but 'renderFrames' needs 'Typeable (StdValM cfg m)' which requires 'Typeable cfg'.
reifyBoolT :: forall r. Bool -> (forall b. (SBoolI b, Typeable b) => Proxy b -> r) -> r
reifyBoolT True  k = k (Proxy @'True)
reifyBoolT False k = k (Proxy @'False)
{-# INLINE reifyBoolT #-}

-- | Type-level configuration record (promoted to kind level via DataKinds).
--
-- Each field corresponds to a runtime configuration option that affects
-- evaluation behavior. By lifting these to the type level, we enable
-- compile-time specialization.
data EvalCfg = MkEvalCfg
  { _cfgStats :: Bool  -- ^ Collect evaluation statistics
  , _cfgProv  :: Bool  -- ^ Track provenance information
  , _cfgTrace :: Bool  -- ^ Enable expression tracing
  }

-- | Default configuration with all flags disabled.
-- Use this for tests and simple usage where no special features are needed.
type DefaultCfg = 'MkEvalCfg 'False 'False 'False

-- | Extract the stats flag from a configuration.
type family CfgStats (cfg :: EvalCfg) :: Bool where
  CfgStats ('MkEvalCfg s _ _) = s

-- | Extract the provenance flag from a configuration.
type family CfgProv (cfg :: EvalCfg) :: Bool where
  CfgProv ('MkEvalCfg _ p _) = p

-- | Extract the trace flag from a configuration.
type family CfgTrace (cfg :: EvalCfg) :: Bool where
  CfgTrace ('MkEvalCfg _ _ t) = t

-- | Constraint that all configuration flags are known at compile time.
--
-- Functions with this constraint can use 'singStats', 'singProv', and
-- 'singTrace' for zero-cost conditional execution.
--
-- Includes 'Typeable cfg' because renderFrames requires @Typeable v@ where
-- @v = StdValM cfg m@, which needs @Typeable cfg@.
type KnownEvalCfg cfg =
  ( SBoolI (CfgStats cfg)
  , SBoolI (CfgProv cfg)
  , SBoolI (CfgTrace cfg)
  , Typeable cfg
  )

-- | Constraint for functions that only need stats flag.
-- Use this instead of 'KnownEvalCfg' to minimize constraints.
type HasStatsCfg cfg = SBoolI (CfgStats cfg)

-- | Constraint for functions that only need provenance flag.
-- Use this instead of 'KnownEvalCfg' to minimize constraints.
type HasProvCfg cfg = SBoolI (CfgProv cfg)

-- | Constraint for functions that only need trace flag.
-- Use this instead of 'KnownEvalCfg' to minimize constraints.
type HasTraceCfg cfg = SBoolI (CfgTrace cfg)

-- | Access the stats singleton for compile-time dispatch.
--
-- @
-- case singStats \@cfg of
--   STrue  -> collectStats
--   SFalse -> pure ()  -- Eliminated by GHC when CfgStats cfg ~ 'False
-- @
singStats :: forall cfg. HasStatsCfg cfg => SBool (CfgStats cfg)
singStats = sbool @(CfgStats cfg)
{-# INLINE singStats #-}

-- | Access the provenance singleton for compile-time dispatch.
--
-- When 'CfgProv cfg ~ 'False', the 'STrue' branch is eliminated entirely
-- by GHC's case-of-known-constructor optimization.
singProv :: forall cfg. HasProvCfg cfg => SBool (CfgProv cfg)
singProv = sbool @(CfgProv cfg)
{-# INLINE singProv #-}

-- | Access the trace singleton for compile-time dispatch.
singTrace :: forall cfg. HasTraceCfg cfg => SBool (CfgTrace cfg)
singTrace = sbool @(CfgTrace cfg)
{-# INLINE singTrace #-}

-- | Zero-cost conditional based on singleton bool.
--
-- @
-- ifSBool STrue  trueVal falseVal = trueVal
-- ifSBool SFalse trueVal falseVal = falseVal
-- @
--
-- GHC eliminates the unused branch when the singleton is known at compile time.
ifSBool :: SBool b -> a -> a -> a
ifSBool STrue  t _ = t
ifSBool SFalse _ f = f
{-# INLINE ifSBool #-}

-- | Zero-cost conditional on stats flag.
--
-- @
-- ifStats \@cfg trueVal falseVal
-- @
--
-- When @CfgStats cfg ~ 'False@, the @trueVal@ is eliminated at compile time.
ifStats :: forall cfg a. HasStatsCfg cfg => a -> a -> a
ifStats = ifSBool (singStats @cfg)
{-# INLINE ifStats #-}

-- | Zero-cost conditional on provenance flag.
ifProv :: forall cfg a. HasProvCfg cfg => a -> a -> a
ifProv = ifSBool (singProv @cfg)
{-# INLINE ifProv #-}

-- | Zero-cost conditional on trace flag.
ifTrace :: forall cfg a. HasTraceCfg cfg => a -> a -> a
ifTrace = ifSBool (singTrace @cfg)
{-# INLINE ifTrace #-}

-- | Execute action only when stats enabled (zero-cost when disabled).
whenStatsM :: forall cfg m. (HasStatsCfg cfg, Applicative m) => m () -> m ()
whenStatsM action = ifStats @cfg action (pure ())
{-# INLINE whenStatsM #-}

-- | Execute action only when provenance enabled (zero-cost when disabled).
whenProvM :: forall cfg m. (HasProvCfg cfg, Applicative m) => m () -> m ()
whenProvM action = ifProv @cfg action (pure ())
{-# INLINE whenProvM #-}

-- | Execute action only when tracing enabled (zero-cost when disabled).
whenTraceM :: forall cfg m. (HasTraceCfg cfg, Applicative m) => m () -> m ()
whenTraceM action = ifTrace @cfg action (pure ())
{-# INLINE whenTraceM #-}

-- | Bridge runtime booleans to type-level configuration.
--
-- This function should be called ONCE at program startup to convert
-- runtime configuration options into a type-level 'EvalCfg'. All subsequent
-- evaluation uses the single @cfg@ type parameter, enabling compile-time
-- specialization.
--
-- @
-- main = do
--   opts <- parseOptions
--   withEvalCfg
--     (isEvalStats opts)
--     (isValues opts)
--     (isTrace opts)
--     $ \\(_ :: Proxy cfg) -> do
--         -- From here, use \@cfg type application
--         runEvaluation \@cfg ...
-- @
--
-- The CPS style ensures that the type-level configuration is available
-- throughout the entire evaluation scope.
withEvalCfg
  :: Bool  -- ^ Collect stats
  -> Bool  -- ^ Track provenance
  -> Bool  -- ^ Enable tracing
  -> (forall cfg. KnownEvalCfg cfg => Proxy cfg -> r)
  -> r
withEvalCfg stats prov trace k =
  reifyBoolT stats $ \(_ :: Proxy s) ->
    reifyBoolT prov $ \(_ :: Proxy p) ->
      reifyBoolT trace $ \(_ :: Proxy t) ->
        k (Proxy @('MkEvalCfg s p t))
{-# INLINE withEvalCfg #-}
