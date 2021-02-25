{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}



module Nix.Thunk.Basic (NThunkF(..), Deferred(..), MonadBasicThunk) where

import           Control.Exception       hiding ( catch )
import           Control.Monad.Catch

import           Nix.Thunk
import           Nix.Var
import Data.Bool (bool)

data Deferred m v = Deferred (m v) | Computed v
    deriving (Functor, Foldable, Traversable)

-- | The type of very basic thunks
data NThunkF m v
    = Thunk (ThunkId m) (Var m Bool) (Var m (Deferred m v))

instance (Eq v, Eq (ThunkId m)) => Eq (NThunkF m v) where
  Thunk x _ _ == Thunk y _ _ = x == y

instance Show v => Show (NThunkF m v) where
  show Thunk{} = "<thunk>"

type MonadBasicThunk m = (MonadThunkId m, MonadVar m)

instance (MonadBasicThunk m, MonadCatch m)
  => MonadThunk (NThunkF m v) m v where
  thunk = buildThunk
  thunkId (Thunk n _ _) = n
  queryM   = queryThunk
  force    = forceThunk
  forceEff = forceEffects
  further  = furtherThunk

buildThunk :: MonadBasicThunk m => m v -> m (NThunkF m v)
buildThunk action = do
  freshThunkId <- freshId
  Thunk freshThunkId <$> newVar False <*> newVar (Deferred action)

--  2021-02-25: NOTE: Please, look into thread handling of this.
-- Locking system was not implemented at the time.
-- How query operates? Is it normal that query on request if the thunk is locked - returns the thunk
-- and when the value calculation is deferred - returns the thunk, it smells fishy.
-- And because the query's impemetation are not used, only API - they pretty much could survive being that fishy.
queryThunk :: MonadVar m
  => (v -> m a)
  -> m a
  -> NThunkF m v
  -> m a
queryThunk k n (Thunk _ active ref) = do
  thunkIsAvaliable <- not <$> atomicModifyVar active (True, )
  bool
    n
    go
    thunkIsAvaliable
   where
    go = do
      eres <- readVar ref
      res  <-
        case eres of
          Computed v   -> k v
          Deferred _mv -> n
      _ <- atomicModifyVar active (False, )
      pure res

forceThunk
  :: forall m v a
   . (MonadVar m, MonadThrow m, MonadCatch m, Show (ThunkId m))
  => (v -> m a)
  -> NThunkF m v
  -> m a
forceThunk k (Thunk n active ref) = do
  eres <- readVar ref
  case eres of
    Computed v      -> k v
    Deferred action -> do
      nowActive <- atomicModifyVar active (True, )
      if nowActive
        then throwM $ ThunkLoop $ show n
        else do
          v <- catch action $ \(e :: SomeException) -> do
            _ <- atomicModifyVar active (False, )
            throwM e
          _ <- atomicModifyVar active (False, )
          writeVar ref (Computed v)
          k v

forceEffects :: MonadVar m
  => (v -> m r)
  -> NThunkF m v
  -> m r
forceEffects k (Thunk _ active ref) = do
  nowActive <- atomicModifyVar active (True, )
  if nowActive
    then pure $ error "Loop detected"
    else do
      eres <- readVar ref
      case eres of
        Computed v      -> k v
        Deferred action -> do
          v <- action
          writeVar ref (Computed v)
          _ <- atomicModifyVar active (False, )
          k v

furtherThunk :: MonadVar m
  => (m v -> m v)
  -> NThunkF m v
  -> m (NThunkF m v)
furtherThunk k t@(Thunk _ _ ref) = do
  _ <- atomicModifyVar ref $ \x -> case x of
    Computed _ -> (x, x)
    Deferred d -> (Deferred (k d), x)
  pure t
