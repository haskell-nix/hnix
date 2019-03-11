{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Nix.Thunk.Basic where

import Control.Exception hiding (catch)
import Control.Monad.Catch

import Nix.Fresh
import Nix.Thunk
import Nix.Utils
import Nix.Var

data Deferred m v = Deferred (m v) | Computed v
    deriving (Functor, Foldable, Traversable)

-- | The type of very basic thunks
data NThunkF m v
    = Value v
    | Thunk Int (Var m Bool) (Var m (Deferred m v))

instance Show v => Show (NThunkF m v) where
    show (Value v) = show v
    show (Thunk _ _ _) = "<thunk>"

valueRef :: v -> NThunkF m v
valueRef = Value

buildThunk :: (MonadVar m, MonadFreshId Int m) => m v -> m (NThunkF m v)
buildThunk action =do
    freshThunkId <- freshId
    Thunk freshThunkId <$> newVar False <*> newVar (Deferred action)

forceThunk :: (MonadVar m, MonadThrow m, MonadCatch m)
           => NThunkF m v -> (v -> m a) -> m a
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

forceEffects :: MonadVar m => NThunkF m v -> (v -> m a) -> m a
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

thunkValue :: NThunkF m v -> Maybe v
thunkValue (Value v) = Just v
thunkValue _ = Nothing
