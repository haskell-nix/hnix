{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

-- | Thunks where the computed values are stored separately rather than in
-- mutable variables
module Nix.Thunk.Separate (NThunkF, MonadSeparateThunk) where

import Control.Exception hiding (catch)
import Control.Monad.Catch
import Control.Monad.Ref
import Data.GADT.Compare
import Control.Monad.State.Strict
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Nix.Thunk.StableId

import Nix.Fresh
import Nix.Thunk
import Nix.Utils
import Nix.Var

-- | The type of very basic thunks
data NThunkF m v
    = Value v
    | Thunk Int (m v)

instance Show v => Show (NThunkF m v) where
    show (Value v) = show v
    show (Thunk _ _) = "<thunk>"

type MonadSeparateThunk m
    = (MonadAtomicRef m, GEq (Ref m), MonadFreshId Int m, MonadCatch m)

--TODO: Make these things weak?
data SeparateThunkState v = SeparateThunkState
  { _separateThunkState_active :: !IntSet
  , _separateThunkState_computed :: !(IntMap v)
  }

instance (MonadAtomicRef m, GEq (Ref m), MonadState (SeparateThunkState v) m, MonadFreshId Int m, MonadCatch m)
  => MonadThunk (NThunkF m v) m v where
    thunk     = buildThunk
    thunkId   = \case
        Value _   -> -1
        Thunk n _ -> n
    query     = queryValue
    queryM    = queryThunk
    force     = forceThunk
    forceEff  = forceEffects
    wrapValue = valueRef
    getValue  = thunkValue

valueRef :: v -> NThunkF m v
valueRef = Value

thunkValue :: NThunkF m v -> Maybe v
thunkValue (Value v) = Just v
thunkValue _ = Nothing

buildThunk :: (MonadFreshId Int m) => m v -> m (NThunkF m v)
buildThunk action =do
    freshThunkId <- freshId
    pure $ Thunk freshThunkId action

queryValue :: (MonadThrow m, MonadCatch m)
           => NThunkF m v -> a -> (v -> a) -> a
queryValue (Value v) _ k = k v
queryValue _ n _ = n

queryThunk :: (MonadState (SeparateThunkState v) m, MonadThrow m, MonadCatch m)
           => NThunkF m v -> m a -> (v -> m a) -> m a
queryThunk (Value v) _ k = k v
queryThunk (Thunk tid _) n k = do
    initialState <- get
    if tid `IntSet.member` _separateThunkState_active initialState then n else do
        put $ initialState { _separateThunkState_active = IntSet.insert tid $ _separateThunkState_active initialState }
        result <- case IntMap.lookup tid $ _separateThunkState_computed initialState of
            Just v -> k v
            Nothing -> n
        modify $ \s -> s { _separateThunkState_active = IntSet.delete tid $ _separateThunkState_active s }
        pure result

forceThunk :: (MonadState (SeparateThunkState v) m, MonadThrow m, MonadCatch m)
           => NThunkF m v -> (v -> m a) -> m a
forceThunk (Value v) k = k v
forceThunk (Thunk tid action) k = do
    initialState <- get
    if tid `IntSet.member` _separateThunkState_active initialState
        then throwM $ ThunkLoop tid
        else do
            put $ initialState { _separateThunkState_active = IntSet.insert tid $ _separateThunkState_active initialState }
            traceM $ "Forcing " ++ show tid
            result <- case IntMap.lookup tid $ _separateThunkState_computed initialState of
                Just v -> k v
                Nothing -> do
                    let setNotActive = modify $ \s -> s
                          { _separateThunkState_active = IntSet.delete tid $ _separateThunkState_active s
                          }
                    v <- catch action $ \(e :: SomeException) -> do
                        _ <- setNotActive
                        throwM e
                    _ <- setNotActive
                    modify $ \s -> s { _separateThunkState_computed = IntMap.insert tid v $ _separateThunkState_computed s }
                    k v
            modify $ \s -> s { _separateThunkState_active = IntSet.delete tid $ _separateThunkState_active s }
            pure result

forceEffects :: (MonadState (SeparateThunkState v) m, MonadVar m) => NThunkF m v -> (v -> m a) -> m a
forceEffects (Value v) k = k v
forceEffects (Thunk tid action) k = do
    initialState <- get
    if tid `IntSet.member` _separateThunkState_active initialState
        then return $ error "forceEffects: a value was expected"
        else do
            put $ initialState { _separateThunkState_active = IntSet.insert tid $ _separateThunkState_active initialState }
            traceM $ "Forcing " ++ show tid
            result <- case IntMap.lookup tid $ _separateThunkState_computed initialState of
                Just v -> k v
                Nothing -> do
                    let setNotActive = modify $ \s -> s
                          { _separateThunkState_active = IntSet.delete tid $ _separateThunkState_active s
                          }
                    v <- action
                    _ <- setNotActive
                    modify $ \s -> s { _separateThunkState_computed = IntMap.insert tid v $ _separateThunkState_computed s }
                    k v
            modify $ \s -> s { _separateThunkState_active = IntSet.delete tid $ _separateThunkState_active s }
            pure result
