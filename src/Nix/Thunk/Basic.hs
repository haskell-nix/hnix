{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Nix.Thunk.Basic (NThunkF(..), Deferred(..), MonadBasicThunk) where

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

instance Eq v => Eq (NThunkF m v) where
    Value x == Value y = x == y
    Thunk x _ _ == Thunk y _ _ = x == y
    _ == _ = False              -- jww (2019-03-16): not accurate...

instance Show v => Show (NThunkF m v) where
    show (Value v) = show v
    show (Thunk _ _ _) = "<thunk>"

type MonadBasicThunk m = (MonadFreshId Int m, MonadVar m)

instance (MonadBasicThunk m, MonadCatch m)
  => MonadThunk (NThunkF m v) m v where
    thunk     = buildThunk
    thunkId   = \case
        Value _     -> -1
        Thunk n _ _ -> n
    query     = queryValue
    queryM    = queryThunk
    force     = forceThunk
    forceEff  = forceEffects
    wrapValue = valueRef
    getValue  = thunkValue

valueRef :: v -> NThunkF m v
valueRef = Value

thunkValue :: NThunkF m v -> Maybe v
thunkValue (Value v) = Just v
thunkValue _ = Nothing

buildThunk :: MonadBasicThunk m => m v -> m (NThunkF m v)
buildThunk action =do
    freshThunkId <- freshId
    Thunk freshThunkId <$> newVar False <*> newVar (Deferred action)

queryValue :: MonadVar m => NThunkF m v -> a -> (v -> a) -> a
queryValue (Value v) _ k = k v
queryValue _ n _ = n

queryThunk :: MonadVar m => NThunkF m v -> m a -> (v -> m a) -> m a
queryThunk (Value v) _ k = k v
queryThunk (Thunk _ active ref) n k = do
    nowActive <- atomicModifyVar active (True,)
    if nowActive
        then n
        else do
            eres <- readVar ref
            res <- case eres of
                Computed v -> k v
                _ -> n
            _ <- atomicModifyVar active (False,)
            return res

forceThunk :: (MonadVar m, MonadThrow m, MonadCatch m)
           => NThunkF m v -> (v -> m a) -> m a
forceThunk (Value v) k = k v
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

forceEffects :: MonadVar m => NThunkF m v -> (v -> m ()) -> m ()
forceEffects (Value v) k = k v
forceEffects (Thunk _ active ref) k = do
    nowActive <- atomicModifyVar active (True,)
    if nowActive
        then return ()
        else do
            eres <- readVar ref
            case eres of
                Computed v -> k v
                Deferred action -> do
                    v <- action
                    writeVar ref (Computed v)
                    _ <- atomicModifyVar active (False,)
                    k v
