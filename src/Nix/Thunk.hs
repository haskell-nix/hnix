{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module Nix.Thunk where

import {-# SOURCE #-} Nix.Stack

data Deferred m v
    = Deferred (m v)
    | Computed v

class Monad m => MonadVar m where
    type Var m :: * -> *
    newVar :: a -> m (Var m a)
    readVar :: Var m a -> m a
    writeVar :: Var m a -> a -> m ()
    atomicModifyVar :: Var m a -> (a -> (a, b)) -> m b

class Monad m => MonadThunk v t m | m -> v, v -> t where
    thunk :: m v -> m t
    force :: t -> (v -> m r) -> m r
    value :: v -> t

data Thunk m v
    = Value v
    | Thunk (Var m Bool) (Var m (Deferred m v))

valueRef :: v -> Thunk m v
valueRef = Value

buildThunk :: MonadVar m => m v -> m (Thunk m v)
buildThunk action =
    Thunk <$> newVar False <*> newVar (Deferred action)

forceThunk :: (Framed e m, MonadFile m, MonadVar m)
           => Thunk m v -> (v -> m r) -> m r
forceThunk (Value ref) k = k ref
forceThunk (Thunk active ref) k = do
    eres <- readVar ref
    case eres of
        Computed v -> k v
        Deferred action -> do
            nowActive <- atomicModifyVar active (True,)
            if nowActive
                then throwError "<<loop>>"
                else do
                    v <- action
                    writeVar ref (Computed v)
                    _ <- atomicModifyVar active (False,)
                    k v

forceEffects :: (Framed e m, MonadFile m, MonadVar m)
             => Thunk m v -> (v -> m r) -> m r
forceEffects (Value ref) k = k ref
forceEffects (Thunk active ref) k = do
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
