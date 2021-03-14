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

-- | It is a reference (@ref-tf: Ref m@), and as such also holds @Bool@ lock.
type ThunkRef m = (Var m Bool)

-- | Reference (@ref-tf: Ref m v@) to a value that thunk holds.
type ThunkValueRef m v = Var m (Deferred m v)

-- | @ref-tf@ lock instruction for @Ref m@ (@ThunkRef@).
lock :: Bool -> (Bool, Bool)
lock = (True, )

-- | @ref-tf@ unlock instruction for @Ref m@ (@ThunkRef@).
unlock :: Bool -> (Bool, Bool)
unlock = (False, )

-- * Data type for thunks: @NThunkF@

-- | The type of very basic thunks
data NThunkF m v
  = Thunk (ThunkId m) (ThunkRef m) (ThunkValueRef m v)

instance (Eq v, Eq (ThunkId m)) => Eq (NThunkF m v) where
  Thunk x _ _ == Thunk y _ _ = x == y

instance Show (NThunkF m v) where
  show Thunk{} = "<thunk>"

type MonadBasicThunk m = (MonadThunkId m, MonadVar m)


-- ** @instance MonadThunk NThunkF@

instance (MonadBasicThunk m, MonadCatch m)
  => MonadThunk (NThunkF m v) m v where

  -- | Return thunk ID
  thunkId :: NThunkF m v -> ThunkId m
  thunkId (Thunk n _ _) = n

  -- | Create new thunk
  thunk :: m v -> m (NThunkF m v)
  thunk action =
    do
      freshThunkId <- freshId
      Thunk freshThunkId <$> newVar False <*> newVar (Deferred action)

  -- | Non-blocking query, return value if @Computed@,
  -- return first argument otherwise.
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

-- | If @m v@ is @Computed@ - returns is
forceMain
  :: ( MonadBasicThunk m
    , MonadCatch m
    )
  => NThunkF m v
  -> m v
forceMain (Thunk n thunkRef thunkValRef) =
  do
    deferred
      pure
      (\ action ->
        do
          lockedIt <- atomicModifyVar thunkRef lock
          bool
            (throwM $ ThunkLoop $ show n)
            (do
              v <- catch action $ \(e :: SomeException) ->
                do
                  _unlockedIt <- atomicModifyVar thunkRef unlock
                  throwM e
              writeVar thunkValRef (Computed v)
              _unlockedIt <- atomicModifyVar thunkRef unlock
              pure v
            )
            (not lockedIt)
      )
      =<< readVar thunkValRef
{-# inline forceMain #-} -- it is big function, but internal, and look at its use.



-- ** Kleisli functor HOFs: @instance MonadThunkF NThunkF@

instance (MonadBasicThunk m, MonadCatch m)
  => MonadThunkF (NThunkF m v) m v where

  queryMF
    :: (v -> m r)
    -> m r
    -> NThunkF m v
    -> m r
  queryMF k n (Thunk _ thunkRef thunkValRef) =
    do
      lockedIt <- atomicModifyVar thunkRef (True, )
      bool
        n
        go
        (not lockedIt)
    where
      go =
        do
          eres <- readVar thunkValRef
          res  <-
            deferred
              k
              (const n)
              eres
          _unlockedIt <- atomicModifyVar thunkRef (False, )
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
      _modifiedIt <- atomicModifyVar ref $
        \x ->
          deferred
            (const (x, x))
            (\ d -> (Deferred (k d), x))
            x
      pure t


-- ** Utils

-- | @either@ for @Deferred@ data type
deferred :: (v -> b) -> (m v -> b) -> Deferred m v -> b
deferred f1 f2 def =
  case def of
    Computed v -> f1 v
    Deferred action -> f2 action
{-# inline deferred #-}
