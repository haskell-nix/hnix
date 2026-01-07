{-# language AllowAmbiguousTypes #-}
{-# language ConstraintKinds #-}
{-# language DataKinds #-}
{-# language TypeFamilies #-}
{-# language RankNTypes #-}

-- | Code for normalization (reduction into a normal form) of Nix expressions.
-- Nix language allows recursion, so some expressions do not converge.
-- And so do not converge into a normal form.
module Nix.Normal where

import           Nix.Prelude
import           Control.Monad.Free        ( Free(..) )
import           Data.Set                  ( member
                                           , insert
                                           )
import           Nix.Cited
import           Nix.Frames
import           Nix.Thunk
import           Nix.Value

newtype NormalLoop t f m = NormalLoop (NValue t f m)
  deriving Show

instance MonadDataErrorContext t f m => Exception (NormalLoop t f m)

-- | Normalize the value as much as possible, leaving only detected cycles.
normalizeValue
  :: forall e t m f
   . ( Framed e m
     , MonadThunk t m (NValue t f m)
     , MonadDataErrorContext t f m
     , Ord (ThunkId m)
     )
  => NValue t f m
  -> m (NValue t f m)
normalizeValue v = run $ iterNValueM run go (fmap Free . sequenceNValue' run) v
 where
  start = 0 :: Int
  maxDepth = 2000
  table = mempty

  run :: ReaderT Int (StateT (Set (ThunkId m)) m) r -> m r
  run = (`evalStateT` table) . (`runReaderT` start)

  go
    :: (  NValue t f m
       -> ReaderT Int (StateT (Set (ThunkId m)) m) (NValue t f m)
       )
    -> t
    -> ReaderT Int (StateT (Set (ThunkId m)) m) (NValue t f m)
  go k tnk  =
    bool
      (do
        i <- ask
        when (i > maxDepth) $ fail $ "Exceeded maximum normalization depth of " <> show maxDepth <> " levels."
        (lifted . lifted)
          (=<< force tnk)
          (local (+1) . k)
      )
      (pure $ pure tnk)
      =<< seen tnk
   where
    seen :: t -> ReaderT Int (StateT (Set (ThunkId m)) m) Bool
    seen t =
      do
        let tnkid = thunkId t
        lift $
          do
            thunkWasVisited <- gets $ member tnkid
            when (not thunkWasVisited) $ modify $ insert tnkid
            pure thunkWasVisited

-- 2021-05-09: NOTE: This seems a bit excessive. If these functorial versions are not used for recursion schemes - just free from it.
-- | Normalization HOF (functorial) version of @normalizeValue@. Accepts the special thunk operating/forcing/nirmalizing function & internalizes it.
normalizeValueF
  :: forall e t m f
   . ( Framed e m
     , MonadThunk t m (NValue t f m)
     , MonadDataErrorContext t f m
     , Ord (ThunkId m)
     )
  => (forall r . t -> (NValue t f m -> m r) -> m r)
  -> NValue t f m
  -> m (NValue t f m)
normalizeValueF f = run . iterNValueM run go (fmap Free . sequenceNValue' run)
 where
  start = 0 :: Int
  maxDepth = 2000
  table = mempty

  run :: ReaderT Int (StateT (Set (ThunkId m)) m) r -> m r
  run = (`evalStateT` table) . (`runReaderT` start)

  go
    :: (  NValue t f m
       -> ReaderT Int (StateT (Set (ThunkId m)) m) (NValue t f m)
       )
    -> t
    -> ReaderT Int (StateT (Set (ThunkId m)) m) (NValue t f m)
  go k tnk  =
    bool
      (do
        i <- ask
        when (i > maxDepth) $ fail $ "Exceeded maximum normalization depth of " <> show maxDepth <> " levels."
        (lifted . lifted)
          (f tnk)
          (local (+1) . k)
      )
      (pure $ pure tnk)
      =<< seen tnk
   where
    seen :: t -> ReaderT Int (StateT (Set (ThunkId m)) m) Bool
    seen t =
      do
        let tnkid = thunkId t
        lift $
          do
            thunkWasVisited <- gets $ member tnkid
            when (not thunkWasVisited) $ modify $ insert tnkid
            pure thunkWasVisited

-- | Normalize value.
-- Detect cycles.
-- If cycles were detected - put a stub on them.
normalForm
  :: ( Framed e m
     , MonadThunk t m (NValue t f m)
     , MonadDataErrorContext t f m
     , HasCitations m (NValue t f m) t
     , HasCitations1 m (NValue t f m) f
     , Ord (ThunkId m)
     )
  => NValue t f m
  -> m (NValue t f m)
normalForm t = stubCycles <$> normalizeValue t

-- | Monadic context of the result.
normalForm_
  :: ( Framed e m
     , MonadThunk t m (NValue t f m)
     , MonadDataErrorContext t f m
     , Ord (ThunkId m)
     )
  => NValue t f m
  -> m ()
normalForm_ t = void $ normalizeValue t

opaqueVal :: NVConstraint f => NValue t f m
opaqueVal = mkNVStrWithoutContext "<cycle>"

-- | Detect cycles & stub them.
stubCycles
  :: forall t f m
   . ( MonadDataContext f m
     , HasCitations m (NValue t f m) t
     , HasCitations1 m (NValue t f m) f
     )
  => NValue t f m
  -> NValue t f m
stubCycles =
  iterNValue
    (\_ t ->
      Free $
        NValue' $
          foldl'
            (flip $ addProvenance1 @m @(NValue t f m))
            cyc
            (citations @m @(NValue t f m) t)
    )
    Free
 where
  Free (NValue' cyc) = opaqueVal

thunkStubVal :: NVConstraint f => NValue t f m
thunkStubVal = mkNVStrWithoutContext thunkStubText

-- | Check if thunk @t@ is computed,
-- then bind it into first arg.
-- else bind the thunk stub val.
bindComputedThunkOrStub
  :: ( NVConstraint f
    , MonadThunk t m (NValue t f m)
    )
  => (NValue t f m -> m a)
  -> t
  -> m a
bindComputedThunkOrStub = (<=< query (pure thunkStubVal))

-- | Traverse a value, replacing uncomputed thunks with @\<thunk\>@ stubs.
--
-- Unlike 'normalizeValue', this function:
--
--   * Does NOT track visited nodes (no @Ord (ThunkId m)@ constraint)
--   * Does NOT force thunks - only examines already-computed values via 'query'
--   * Uses depth limiting (200) instead of cycle detection
--
-- The depth limit is necessary because self-referential structures can exist
-- in computed values (e.g., derivations where @drv.out = drv@). Without cycle
-- tracking, following such references would recurse forever.
--
-- This trades precision for generality - it works in more contexts but may
-- truncate legitimate deep structures at the arbitrary depth limit.
removeEffects
  :: forall t f m . (MonadThunk t m (NValue t f m), MonadDataContext f m)
  => NValue t f m
  -> m (NValue t f m)
removeEffects = go 0
 where
  maxDepth :: Int
  maxDepth = 200

  go :: Int -> NValue t f m -> m (NValue t f m)
  go depth v
    | depth > maxDepth = pure $ mkNVStrWithoutContext "<deep>"
    | otherwise = case v of
        Pure t -> bindComputedThunkOrStub (go (depth + 1)) t
        Free fv -> Free <$> bindNValue' id (go (depth + 1)) fv

dethunk
  :: (MonadThunk t m (NValue t f m), MonadDataContext f m)
  => t
  -> m (NValue t f m)
dethunk = bindComputedThunkOrStub removeEffects
