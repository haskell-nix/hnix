{-# language ConstraintKinds #-}
{-# language UndecidableInstances #-}
{-# options_ghc -Wno-unused-do-bind #-}

-- | Basic thunk implementation for lazy evaluation with memoization.
--
-- == Performance Notes (from profiling ~1M thunks)
--
-- === Flattened Representation (13.6% speedup)
-- Original design used 2 IORefs per thunk (lock + value). Flattening into
-- a single IORef with 'ThunkState' reduced:
--   * Allocations: 64.8 GB → 61.5 GB (-5%)
--   * Runtime: 140s → 121s (-13.6%)
--   * IORef operations per force: 4 → 2
--
-- === Strict Extension is Critical
-- Without strict evaluation, thunks accumulate inside the IORef, causing
-- space leaks. The @Strict@ extension (controlled by the @strict@ cabal flag)
-- ensures all bindings and data fields are strict by default.
--
-- === atomicModifyRef vs readRef for Cache Hits
-- Attempted optimization: use 'readRef' for cache hits (64.7% of forces),
-- 'atomicModifyRef' only for misses. Result: 5s SLOWER.
-- Why: The extra 'readRef' for cache misses (35.3%) costs more than
-- avoiding 'atomicModifyRef' for hits. Single 'atomicModifyRef' is optimal.
--
-- === Key Metrics (nixpkgs hello.name benchmark)
--   * Thunks created: ~908K
--   * Forces: ~1M (64.7% cache hits)
--   * GC overhead: only 3.5% (not the bottleneck)
--   * Remaining bottleneck: allocation rate (514 MB/s)

module Nix.Thunk.Basic
  ( NThunkF(..)
  , ThunkState(..)
  , thunkState
  , isComputed
  , MonadBasicThunk
  ) where

import           Nix.Prelude
import           Control.Monad.Ref              ( MonadRef(Ref, newRef, readRef, writeRef)
                                                , MonadAtomicRef(atomicModifyRef)
                                                )
import           Control.Monad.Catch            ( MonadCatch(..)
                                                , MonadThrow(throwM)
                                                )
import qualified Text.Show
import           Nix.Thunk


-- * Flattened thunk state (combines lock + value into single IORef)

-- | Thunk state with integrated lock for loop detection.
--
-- This flattened design reduces from 2 IORefs to 1 per thunk:
--
-- @
-- -- OLD (2 IORefs):
-- data NThunkF m v = Thunk !(ThunkId m) !(IORef Bool) !(IORef (Deferred m v))
--
-- -- NEW (1 IORef):
-- data NThunkF m v = Thunk !(ThunkId m) !(IORef (ThunkState m v))
-- @
--
-- State transitions:
--
-- @
-- ThunkDeferred action  --[force]-->  ThunkComputing action  --[done]-->  ThunkComputed v
--                                            |
--                                            +--[force again = LOOP ERROR]
-- @
--
-- All fields are strict (via Strict pragma) to avoid space leaks.
data ThunkState m v
  = ThunkComputed v       -- ^ Fully evaluated value
  | ThunkDeferred (m v)   -- ^ Not yet evaluated, stores the computation
  | ThunkComputing (m v)  -- ^ Currently being evaluated, for loop detection
  deriving (Functor, Foldable, Traversable)

-- | Pattern match on thunk state (analog of 'either' for ThunkState)
thunkState
  :: (v -> b)      -- ^ Handler for computed value
  -> (m v -> b)    -- ^ Handler for deferred computation
  -> (m v -> b)    -- ^ Handler for computing (in-progress)
  -> ThunkState m v
  -> b
thunkState onComputed onDeferred onComputing = \case
  ThunkComputed v   -> onComputed v
  ThunkDeferred mv  -> onDeferred mv
  ThunkComputing mv -> onComputing mv
{-# INLINABLE thunkState #-}


-- * Data type for thunks: @NThunkF@ (flattened)

-- | Reference to thunk state (single IORef instead of 2)
type ThunkStateRef m v = Ref m (ThunkState m v)

-- | The type of very basic thunks.
-- Uses a single IORef for both lock state and value (flattened from 2 IORefs).
data NThunkF m v =
  Thunk (ThunkId m) (ThunkStateRef m v)

instance (Eq v, Eq (ThunkId m)) => Eq (NThunkF m v) where
  Thunk x _ == Thunk y _ = x == y

instance Show (NThunkF m v) where
  show Thunk{} = toString thunkStubText

-- | Check if a thunk value is already computed (cache hit detection)
isComputed :: MonadRef m => NThunkF m v -> m Bool
isComputed (Thunk _ stateRef) = do
  s <- readRef stateRef
  pure $ case s of
    ThunkComputed _ -> True
    _               -> False
{-# INLINABLE isComputed #-}

type MonadBasicThunk m = (MonadThunkId m, MonadAtomicRef m)


-- ** @instance MonadThunk NThunkF@

instance (MonadBasicThunk m, MonadCatch m)
  => MonadThunk (NThunkF m v) m v where

  thunkId :: NThunkF m v -> ThunkId m
  thunkId (Thunk n _) = n

  thunk :: m v -> m (NThunkF m v)
  thunk action = do
    freshThunkId <- freshId
    stateRef <- newRef $ ThunkDeferred action
    pure $ Thunk freshThunkId stateRef
  {-# INLINABLE thunk #-}

  query :: m v -> NThunkF m v -> m v
  query vStub (Thunk _ stateRef) = do
    s <- readRef stateRef
    case s of
      ThunkComputed v -> pure v
      _               -> vStub
  {-# INLINABLE query #-}

  force :: NThunkF m v -> m v
  force = forceMain
  {-# INLINABLE force #-}

  forceEff :: NThunkF m v -> m v
  forceEff = forceMain
  {-# INLINABLE forceEff #-}

  further :: NThunkF m v -> m (NThunkF m v)
  further t@(Thunk _ ref) = do
    _ <- atomicModifyRef ref $ \s -> (s, ())
    pure t
  {-# INLINABLE further #-}


-- *** Flattened force implementation

-- | Force a thunk to its computed value.
--
-- == Implementation Notes
--
-- Uses single 'atomicModifyRef' for ALL cases (cache hits and misses).
--
-- === Why not use readRef for cache hits?
-- We tried: check with 'readRef' first, only use 'atomicModifyRef' for misses.
-- Result: 5 seconds SLOWER despite 64.7% cache hit rate.
--
-- Reason: For cache misses (35.3%), we'd do readRef + atomicModifyRef (2 ops).
-- The overhead on misses exceeds savings on hits. Single atomicModifyRef wins.
--
-- === Operation count per force:
--   * Cache hit: 1 atomicModifyRef (returns value, no state change)
--   * Cache miss: 1 atomicModifyRef + 1 writeRef + computation
--
-- === Performance
-- ~1M forces in nixpkgs benchmark: 64.7% hits, 35.3% misses
-- Average cache hit: ~12µs (includes Cited wrapper overhead above this layer)
forceMain
  :: forall v m
   . ( MonadBasicThunk m
    , MonadCatch m
    )
  => NThunkF m v
  -> m v
forceMain (Thunk tIdV stateRef) = do
  -- Single atomic operation handles both cache hits and state transitions.
  -- For cache hits, this is effectively just a read (state unchanged).
  -- For cache misses, atomically transitions Deferred -> Computing.
  result <- atomicModifyRef stateRef $ \case
    ThunkComputed v ->
      -- Cache hit: return value, state unchanged
      (ThunkComputed v, Right v)
    ThunkDeferred action ->
      -- Cache miss: transition to Computing, return action to execute
      (ThunkComputing action, Left action)
    ThunkComputing _ ->
      -- Loop detection: already computing this thunk = infinite recursion
      let err = throwM $ ThunkLoop $ show tIdV
      in (ThunkComputing err, Left err)

  case result of
    Right v -> pure v  -- Cache hit - done
    Left action -> do
      -- Cache miss - execute the deferred computation
      v <- action `catch` \(e :: SomeException) -> throwM e
      -- Store result for future cache hits
      writeRef stateRef $ ThunkComputed v
      pure v
{-# INLINABLE forceMain #-}


-- ** Kleisli functor HOFs: @instance MonadThunkF NThunkF@

instance (MonadBasicThunk m, MonadCatch m)
  => MonadThunkF (NThunkF m v) m v where

  queryF
    :: (v -> m r)
    -> m r
    -> NThunkF m v
    -> m r
  queryF k n (Thunk _ stateRef) = do
    s <- readRef stateRef
    case s of
      ThunkComputed v -> k v
      _               -> n
  {-# INLINABLE queryF #-}

  forceF
    :: (v -> m a)
    -> NThunkF m v
    -> m a
  forceF k = k <=< force
  {-# INLINABLE forceF #-}

  forceEffF
    :: (v -> m r)
    -> NThunkF m v
    -> m r
  forceEffF k = k <=< forceEff
  {-# INLINABLE forceEffF #-}

  furtherF
    :: (m v -> m v)
    -> NThunkF m v
    -> m (NThunkF m v)
  furtherF k t@(Thunk _ ref) = do
    _ <- atomicModifyRef ref $ \case
      ThunkDeferred d -> let d' = k d in (ThunkDeferred d', ())
      s               -> (s, ())
    pure t
  {-# INLINABLE furtherF #-}
