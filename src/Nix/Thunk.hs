{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}

#if ENABLE_TRACING
{-# LANGUAGE BangPatterns #-}
#endif

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module Nix.Thunk where

import {-# SOURCE #-} Nix.Stack

#if ENABLE_TRACING
import Data.IORef
import System.IO.Unsafe
import Nix.Utils

counter :: IORef Int
counter = unsafePerformIO $ newIORef 0
{-# NOINLINE counter #-}
#endif

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
    | Thunk
#if ENABLE_TRACING
          !Int
#endif
          (Var m Bool) (Var m (Deferred m v))

valueRef :: v -> Thunk m v
valueRef = Value

buildThunk :: MonadVar m => m v -> m (Thunk m v)
buildThunk action =
#if ENABLE_TRACING
    let !x = unsafePerformIO (atomicModifyIORef' counter (\x -> (succ x, x))) in
    Thunk x
#else
    Thunk
#endif
        <$> newVar False <*> newVar (Deferred action)

forceThunk :: (Framed e m, MonadFile m, MonadVar m)
           => Thunk m v -> (v -> m r) -> m r
forceThunk (Value ref) k = k ref
#if ENABLE_TRACING
forceThunk (Thunk n active ref) k = do
#else
forceThunk (Thunk active ref) k = do
#endif
    eres <- readVar ref
    case eres of
        Computed v -> k v
        Deferred action -> do
            nowActive <- atomicModifyVar active (True,)
            if nowActive
                then
#if ENABLE_TRACING
                    throwError $ "<<loop in #" ++ show n ++ ">>"
#else
                    throwError "<<loop>>"
#endif
                else do
#if ENABLE_TRACING
                    traceM $ "Forcing " ++ show n
#endif
                    v <- action
                    writeVar ref (Computed v)
                    _ <- atomicModifyVar active (False,)
                    k v

forceEffects :: (Framed e m, MonadFile m, MonadVar m)
             => Thunk m v -> (v -> m r) -> m r
forceEffects (Value ref) k = k ref
#if ENABLE_TRACING
forceEffects (Thunk _ active ref) k = do
#else
forceEffects (Thunk active ref) k = do
#endif
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
