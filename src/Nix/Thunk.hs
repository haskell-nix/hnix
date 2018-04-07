{-# LANGUAGE FlexibleContexts #-}
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

data Thunk m v
    = Value v
    | Action (m v)
    | Thunk (Var m Bool) (Var m (Deferred m v))

valueRef :: v -> Thunk m v
valueRef = Value

buildRepeatingThunk :: m v -> Thunk m v
buildRepeatingThunk = Action

buildThunk :: MonadVar m => m v -> m (Thunk m v)
buildThunk action =
    Thunk <$> newVar False <*> newVar (Deferred action)

forceThunk :: (Framed e m, MonadFile m, MonadVar m)
           => Thunk m v -> (v -> m r) -> m r
forceThunk (Value ref) k = k ref
forceThunk (Action ref) k = k =<< ref
forceThunk (Thunk active ref) k = do
    eres <- readVar ref
    case eres of
        Computed value -> k value
        Deferred action -> do
            nowActive <- atomicModifyVar active (True,)
            if nowActive
                then throwError "<<loop>>"
                else do
                    value <- action
                    writeVar ref (Computed value)
                    _ <- atomicModifyVar active (False,)
                    k value

forceEffects :: (Framed e m, MonadFile m, MonadVar m)
             => Thunk m v -> (v -> m r) -> m r
forceEffects (Value ref) k = k ref
forceEffects (Action ref) k = k =<< ref
forceEffects (Thunk active ref) k = do
    nowActive <- atomicModifyVar active (True,)
    if nowActive
        then return $ error "forceEffects: a value was expected"
        else do
            eres <- readVar ref
            case eres of
                Computed value -> k value
                Deferred action -> do
                    value <- action
                    writeVar ref (Computed value)
                    _ <- atomicModifyVar active (False,)
                    k value
