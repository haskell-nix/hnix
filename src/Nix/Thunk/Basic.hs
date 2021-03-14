{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ApplicativeDo #-}



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
import           Nix.Utils                      ( bool
                                                , dup
                                                )

data Deferred m v = Computed v | Deferred (m v)
  deriving (Functor, Foldable, Traversable)

-- * Data type for thunks: @NThunkF@

-- | The type of very basic thunks
data NThunkF m v
  = Thunk (ThunkId m) (Var m Bool) (Var m (Deferred m v))

instance (Eq v, Eq (ThunkId m)) => Eq (NThunkF m v) where
  Thunk x _ _ == Thunk y _ _ = x == y

instance Show (NThunkF m v) where
  show Thunk{} = "<thunk>"

type MonadBasicThunk m = (MonadThunkId m, MonadVar m)


-- ** @instance MonadThunk NThunkF@

instance (MonadBasicThunk m, MonadCatch m)
  => MonadThunk (NThunkF m v) m v where

  thunkId :: NThunkF m v -> ThunkId m
  thunkId (Thunk n _ _) = n

  thunk :: m v -> m (NThunkF m v)
  thunk action =
    do
      freshThunkId <- freshId
      Thunk freshThunkId <$> newVar False <*> newVar (Deferred action)

  -- | Non-blocking query
  queryM :: m v -> NThunkF m v -> m v
  queryM n (Thunk _ _ ref) =
    do
      deferred
        pure
        (const n)
        =<< readVar ref

  force :: NThunkF m v -> m v
  force = forceMain

  forceEff :: NThunkF m v -> m v
  forceEff = forceMain

  further :: NThunkF m v -> m (NThunkF m v)
  further t@(Thunk _ _ ref) =
    do
      _ <-
        atomicModifyVar
          ref
          dup
      pure t


-- *** United body of `force*`

forceMain
  :: ( MonadBasicThunk m
    , MonadCatch m
    )
  => NThunkF m v
  -> m v
forceMain (Thunk n active ref) =
  do
    deferred
      pure
      (\ action ->
        do
          lockThunk <- atomicModifyVar active (True, )
          bool
            (throwM $ ThunkLoop $ show n)
            (do
              v <- catch action $ \(e :: SomeException) ->
                do
                  _ <- atomicModifyVar active (False, )
                  throwM e
              writeVar ref (Computed v)
              _unlockThunk <- atomicModifyVar active (False, )
              pure v
            )
            (not lockThunk)
      )
      =<< readVar ref
{-# inline forceMain #-} -- it is big function, but internal, and look at its use.



-- ** Kleisli functor HOFs: @instance MonadThunkF NThunkF@

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


-- ** Utils

-- | @either@ for @Deferred@ data type
deferred :: (v -> b) -> (m v -> b) -> Deferred m v -> b
deferred f1 f2 def =
  case def of
    Computed v -> f1 v
    Deferred action -> f2 action
{-# inline deferred #-}
