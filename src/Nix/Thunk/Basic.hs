{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE InstanceSigs #-}



module Nix.Thunk.Basic
  ( NThunkF(..)
  , Deferred(..)
  , MonadBasicThunk
  ) where

import           Control.Exception              ( SomeException )
import           Control.Monad                  ( (<=<) )
import           Control.Monad.Catch            ( MonadCatch(..)
                                                , MonadThrow(throwM)
                                                )
import           Nix.Thunk
import           Nix.Var
import           Nix.Utils                      ( bool )

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

  thunkId :: NThunkF m v -> ThunkId m
  thunkId (Thunk n _ _) = n

  thunk :: m v -> m (NThunkF m v)
  thunk action =
    do
      freshThunkId <- freshId
      Thunk freshThunkId <$> newVar False <*> newVar (Deferred action)

  queryM :: m v -> NThunkF m v -> m v
  queryM n (Thunk _ active ref) =
    do
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
            Computed v   -> pure v
            Deferred _mv -> n
        _ <- atomicModifyVar active (False, )
        pure res

  force :: NThunkF m v -> m v
  force (Thunk n active ref) =
    do
      eres <- readVar ref
      case eres of
        Computed v      -> pure v
        Deferred action ->
          do
            nowActive <- atomicModifyVar active (True, )
            bool
              (do
                v <- catch action $ \(e :: SomeException) ->
                  do
                    _ <- atomicModifyVar active (False, )
                    throwM e
                writeVar ref (Computed v)
                _ <- atomicModifyVar active (False, )
                pure v
              )
              (throwM $ ThunkLoop $ show n)
              nowActive

  forceEff :: NThunkF m v -> m v
  forceEff (Thunk _ active ref) =
    do
      nowActive <- atomicModifyVar active (True, )
      bool
        (do
          eres <- readVar ref
          case eres of
            Computed v      -> pure v
            Deferred action ->
              do
                v <- action
                writeVar ref (Computed v)
                _ <- atomicModifyVar active (False, )
                pure v
        )
        (pure $ error "Loop detected")
        nowActive

  further :: NThunkF m v -> m (NThunkF m v)
  further t@(Thunk _ _ ref) = do
    _ <- atomicModifyVar ref $
      \x -> case x of
        Computed _ -> (x, x)
        Deferred d -> (Deferred d, x)
    pure t


-- * Kleisli functor HOFs

instance (MonadBasicThunk m, MonadCatch m)
  => MonadThunkF (NThunkF m v) m v where

  queryMF
    :: (v -> m r)
    -> m r
    -> NThunkF m v
    -> m r
  queryMF k n (Thunk _ active ref) =
    do
      thunkIsAvaliable <- not <$> atomicModifyVar active (True, )
      bool
        n
        go
        thunkIsAvaliable
    where
      go =
        do
          eres <- readVar ref
          res  <-
            case eres of
              Computed v   -> k v
              Deferred _mv -> n
          _ <- atomicModifyVar active (False, )
          pure res

  forceF
    :: (v -> m a)
    -> NThunkF m v
    -> m a
  forceF k = k <=< force

  forceEffF
    :: (v -> m r)
    -> NThunkF m v
    -> m r
  forceEffF k = k <=< forceEff

  furtherF
    :: (m v -> m v)
    -> NThunkF m v
    -> m (NThunkF m v)
  furtherF k t@(Thunk _ _ ref) =
    do
      _ <- atomicModifyVar ref $
        \x -> case x of
          Computed _ -> (x, x)
          Deferred d -> (Deferred (k d), x)
      pure t

