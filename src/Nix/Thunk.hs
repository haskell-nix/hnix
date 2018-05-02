{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
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

import Control.Exception
import Control.Monad.Catch
import Data.Typeable

#if ENABLE_TRACING
import Data.IORef
import System.IO.Unsafe
import Nix.Utils

counter :: IORef Int
counter = unsafePerformIO $ newIORef 0
{-# NOINLINE counter #-}
#endif

data Deferred m v = Deferred (m v) | Computed v
    deriving (Functor, Foldable, Traversable)

class Monad m => MonadVar m where
    type Var m :: * -> *
    newVar :: a -> m (Var m a)
    readVar :: Var m a -> m a
    writeVar :: Var m a -> a -> m ()
    atomicModifyVar :: Var m a -> (a -> (a, b)) -> m b

class Monad m => MonadThunk v t m | v -> m, v -> t, t -> m, t -> v where
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

buildThunk :: MonadVar m => m v -> m (Thunk m v)
buildThunk action =
#if ENABLE_TRACING
    let !x = unsafePerformIO (atomicModifyIORef' counter (\c -> (succ c, c))) in
    Thunk x
#else
    Thunk 0
#endif
        <$> newVar False <*> newVar (Deferred action)

forceThunk :: (MonadVar m, MonadThrow m) => Thunk m v -> (v -> m a) -> m a
forceThunk (Value ref) k = k ref
#if ENABLE_TRACING
forceThunk (Thunk n active ref) k = do
#else
forceThunk (Thunk _ active ref) k = do
#endif
    eres <- readVar ref
    case eres of
        Computed v -> k v
        Deferred action -> do
            nowActive <- atomicModifyVar active (True,)
            if nowActive
                then
#if ENABLE_TRACING
                    throwM $ ThunkLoop (Just n)
#else
                    throwM $ ThunkLoop Nothing
#endif
                else do
#if ENABLE_TRACING
                    traceM $ "Forcing " ++ show n
#endif
                    v <- action
                    writeVar ref (Computed v)
                    _ <- atomicModifyVar active (False,)
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
