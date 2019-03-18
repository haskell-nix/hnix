{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Nix.Thunk.Separate
  ( NThunkF (..)
  , MonadSeparateThunk
  , SeparateThunkT
  , runSeparateThunkT
  , askThunkCache
  , buildThunk
  , separateThunkId
  , queryValue
  , queryThunk
  , forceThunk
  , forceEffects
  , valueRef
  , thunkValue
  , newThunkCache
  , Forcer (..)
  ) where

import Control.Exception hiding (catch)
import Control.Applicative
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.Ref
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Nix.Thunk

-- | The type of very basic thunks
data NThunkF forcer v
    = Value v
    | Thunk (ThunkId forcer) (forcer v)

instance (Eq v, Eq (ThunkId forcer)) => Eq (NThunkF forcer v) where
    Value x == Value y = x == y
    Thunk x _ == Thunk y _ = x == y
    _ == _ = False              -- jww (2019-03-16): not accurate...

instance Show v => Show (NThunkF forcer v) where
    show (Value v) = show v
    show (Thunk _ _) = "<thunk>"

type MonadSeparateThunk m = (MonadThunkId m, MonadAtomicRef m, Ord (ThunkId m)) --TODO: ThunkId allocation also needs to be sufficiently deterministic

newtype ThunkCache m v = ThunkCache (Ref m (Map (ThunkId m) (Maybe v)))

class (Monad forcer, ThunkId forcer ~ ThunkId m) => Forcer forcer v m | forcer -> v, forcer -> m where
  liftSeparateThunkT :: SeparateThunkT forcer v m a -> forcer a

--TODO: HashMap?
newtype SeparateThunkT (forcer :: * -> *) v m a = SeparateThunkT (ReaderT (ThunkCache m v) m a)
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadRef
    , MonadAtomicRef
    , MonadCatch
    , MonadThrow
    , MonadIO
    , MonadPlus
    , Alternative
    , MonadFix
    )

newThunkCache :: (MonadRef m, Ref m ~ Ref m') => m (ThunkCache m' v)
newThunkCache = ThunkCache <$> newRef Map.empty

askThunkCache :: Monad m => SeparateThunkT forcer v m (ThunkCache m v)
askThunkCache = SeparateThunkT ask

runSeparateThunkT :: ThunkCache m v -> SeparateThunkT forcer v m a -> m a
runSeparateThunkT c (SeparateThunkT a) = runReaderT a c

instance MonadTrans (SeparateThunkT forcer v) where
    lift = SeparateThunkT . lift

instance MonadThunkId m => MonadThunkId (SeparateThunkT forcer v m) where
    type ThunkId (SeparateThunkT forcer v m) = ThunkId m

{-
instance (MonadSeparateThunk m, MonadCatch m)
  => MonadThunk (NThunkF forcer v) (SeparateThunkT forcer v m) v where
    thunk     = buildThunk
    thunkId   = \case
        Value _   -> Nothing
        Thunk n _ -> Just n
    query     = queryValue
    queryM    = queryThunk
    force     = forceThunk
    forceEff  = forceEffects
    wrapValue = valueRef
    getValue  = thunkValue
-}

separateThunkId :: NThunkF forcer v -> Maybe (ThunkId forcer)
separateThunkId = \case
  Value _   -> Nothing
  Thunk n _ -> Just n

valueRef :: v -> NThunkF forcer v
valueRef = Value

thunkValue :: NThunkF forcer v -> Maybe v
thunkValue (Value v) = Just v
thunkValue _ = Nothing

buildThunk :: (MonadThunkId m, ThunkId m ~ ThunkId forcer) => forcer v -> SeparateThunkT forcer v m (NThunkF forcer v)
buildThunk action = do
    freshThunkId <- lift freshId
    return $ Thunk freshThunkId action

queryValue :: NThunkF forcer v -> a -> (v -> a) -> a
queryValue (Value v) _ k = k v
queryValue _ n _ = n

queryThunk :: (Forcer forcer v m, Monad m, MonadAtomicRef m, Ord (ThunkId m)) => NThunkF forcer v -> forcer a -> (v -> forcer a) -> forcer a
queryThunk (Value v) _ k = k v
queryThunk (Thunk tid _) n k = do
    ThunkCache c <- liftSeparateThunkT $ SeparateThunkT ask
    mOldVal <- liftSeparateThunkT $ atomicModifyRef' c $ \old ->
        -- Try to insert Nothing into the given key, but if something is already
        -- there, just leave it
        let (mOldVal, !new) = Map.insertLookupWithKey (\_ _ oldVal -> oldVal) tid Nothing old
        in (new, mOldVal)
    case mOldVal of
        Nothing -> do
            result <- n -- Not computed, inactive
            -- This is the only case where we've actually changed c, so restore it
            liftSeparateThunkT $ atomicModifyRef' c $ \old -> (Map.delete tid old, ())
            return result
        Just Nothing -> n -- Active
        Just (Just v) -> k v -- Computed, inactive

forceThunk
    :: forall m v a forcer.
    ( MonadAtomicRef m
    , MonadThrow forcer
    , MonadCatch forcer
    , Show (ThunkId m)
    , Ord (ThunkId m)
    , Forcer forcer v m
    )
    => NThunkF forcer v -> (v -> forcer a) -> forcer a
forceThunk (Value v) k = k v
forceThunk (Thunk tid action) k = do
    ThunkCache c <- liftSeparateThunkT $ SeparateThunkT ask
    mOldVal <- liftSeparateThunkT $ atomicModifyRef' c $ \old ->
        -- Try to insert Nothing into the given key, but if something is already
        -- there, just leave it
        let (mOldVal, !new) = Map.insertLookupWithKey (\_ _ oldVal -> oldVal) tid Nothing old
        in (new, mOldVal)
    case mOldVal of
        Nothing -> do -- Not computed, inactive
            v <- catch action $ \(e :: SomeException) -> do
                -- This is the only case where we've actually changed c, so restore it
                _ <- liftSeparateThunkT $ atomicModifyRef' c $ \old -> (Map.delete tid old, ())
                throwM e
            liftSeparateThunkT $ atomicModifyRef' c $ \old -> (Map.insert tid (Just v) old, ())
            k v
        Just Nothing -> throwM $ ThunkLoop $ show tid
        Just (Just v) -> k v -- Computed, inactive

forceEffects :: (MonadAtomicRef m, Ord (ThunkId m), Forcer forcer v m) => NThunkF forcer v -> (v -> forcer r) -> forcer r
forceEffects (Value v) k = k v
forceEffects (Thunk tid action) k = do
    ThunkCache c <- liftSeparateThunkT $ SeparateThunkT ask
    mOldVal <- liftSeparateThunkT $ atomicModifyRef' c $ \old ->
        -- Try to insert Nothing into the given key, but if something is already
        -- there, just leave it
        let (mOldVal, !new) = Map.insertLookupWithKey (\_ _ oldVal -> oldVal) tid Nothing old
        in (new, mOldVal)
    case mOldVal of
        Nothing -> do -- Not computed, inactive
            v <- action
            liftSeparateThunkT $ atomicModifyRef' c $ \old -> (Map.insert tid (Just v) old, ())
            k v
        Just Nothing -> return $ error "Loop detected"
        Just (Just v) -> k v -- Computed, inactive
