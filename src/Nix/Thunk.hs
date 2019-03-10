{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Nix.Thunk where

import Control.Exception hiding (catch)
import Control.Monad.Catch
import Control.Monad.Ref
import Data.GADT.Compare
import Data.IORef
import Data.Maybe
import Data.STRef
import Data.Typeable

import Unsafe.Coerce

import Nix.Fresh
import Nix.Utils

data Deferred m v = Deferred (m v) | Computed v
    deriving (Functor, Foldable, Traversable)

type Var m = Ref m

--TODO: Eliminate the old MonadVar shims
type MonadVar m =
  ( MonadAtomicRef m
  , GEq (Ref m)
  )

eqVar :: forall m a. GEq (Ref m) => Ref m a -> Ref m a -> Bool
eqVar a b = isJust $ geq a b

newVar :: MonadRef m => a -> m (Ref m a)
newVar = newRef

readVar :: MonadRef m => Ref m a -> m a
readVar = readRef

writeVar :: MonadRef m => Ref m a -> a -> m ()
writeVar = writeRef

atomicModifyVar :: MonadAtomicRef m => Ref m a -> (a -> (a, b)) -> m b
atomicModifyVar = atomicModifyRef

--TODO: Upstream GEq instances
instance GEq IORef where
    a `geq` b = if a == unsafeCoerce b
                then Just $ unsafeCoerce Refl
                else Nothing

instance GEq (STRef s) where
    a `geq` b = if a == unsafeCoerce b
                then Just $ unsafeCoerce Refl
                else Nothing

class Monad m => MonadThunk v t m | m -> t, t -> m, t -> v where
    thunk :: m v -> m t
    force :: t -> (v -> m r) -> m r
    value :: v -> t

data Thunk m v
    = Value v
    | Thunk Int (Var m Bool) (Var m (Deferred m v))

newtype ThunkLoop = ThunkLoop (Maybe Int)
    deriving (Show, Typeable)

instance Exception ThunkLoop

valueRef :: v -> Thunk m v
valueRef = Value

buildThunk :: (MonadVar m, MonadFreshId Int m) => m v -> m (Thunk m v)
buildThunk action =do
    freshThunkId <- freshId
    Thunk freshThunkId <$> newVar False <*> newVar (Deferred action)

forceThunk :: (MonadVar m, MonadThrow m, MonadCatch m)
           => Thunk m v -> (v -> m a) -> m a
forceThunk (Value ref) k = k ref
forceThunk (Thunk n active ref) k = do
    eres <- readVar ref
    case eres of
        Computed v -> k v
        Deferred action -> do
            nowActive <- atomicModifyVar active (True,)
            if nowActive
                then
                    throwM $ ThunkLoop (Just n)
                else do
                    traceM $ "Forcing " ++ show n
                    v <- catch action $ \(e :: SomeException) -> do
                        _ <- atomicModifyVar active (False,)
                        throwM e
                    _ <- atomicModifyVar active (False,)
                    writeVar ref (Computed v)
                    k v

forceEffects :: MonadVar m => Thunk m v -> (v -> m a) -> m a
forceEffects (Value ref) k = k ref
forceEffects (Thunk _ active ref) k = do
    nowActive <- atomicModifyVar active (True,)
    if nowActive
        then return $ error "forceEffects: a value was expected"
        else do
            eres <- readVar ref
            case eres of
                Computed v -> k v
                Deferred action -> do
                    v <- action
                    writeVar ref (Computed v)
                    _ <- atomicModifyVar active (False,)
                    k v
