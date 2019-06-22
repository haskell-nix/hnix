{-# LANGUAGE CPP #-}
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
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Nix.Thunk.Basic (ThunkT (..), NThunkF(..), Deferred(..), MonadBasicThunk) where

import           Control.Exception       hiding ( catch )
import           Control.Monad.Catch

import           Nix.Thunk
import           Nix.Utils
import           Nix.Var
import           Control.Applicative
import           Control.Monad.Base
import           Control.Monad.Catch
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.Ref
import           Control.Monad.ST
import Data.Typeable
#ifdef MIN_VERSION_haskeline
import System.Console.Haskeline.MonadException hiding(catch)
#endif

newtype ThunkT m a = ThunkT { unThunkT :: m a }
  deriving
    ( Functor
    , Applicative
    , Alternative
    , Monad
    , MonadPlus
    , MonadFix
    , MonadRef
    , MonadAtomicRef
    , MonadIO
    , MonadCatch
    , MonadThrow
#ifdef MIN_VERSION_haskeline
    , MonadException
#endif
    )

instance MonadThunkId m => MonadThunkId (ThunkT m) where
  type ThunkId (ThunkT m) = ThunkId m

instance MonadTrans ThunkT where
  lift = ThunkT

instance MonadTransWrap ThunkT where
  liftWrap f (ThunkT a) = ThunkT $ f a

data Deferred m v = Deferred (m v) | Computed v
    deriving (Functor, Foldable, Traversable)

-- | The type of very basic thunks
data NThunkF m v
    = Thunk (ThunkId m) (Var m Bool) (Var m (Deferred m v))

instance (Eq v, Eq (ThunkId m)) => Eq (NThunkF m v) where
  Thunk x _ _ == Thunk y _ _ = x == y

instance Show v => Show (NThunkF m v) where
  show (Thunk _ _ _) = "<thunk>"

type MonadBasicThunk m = (MonadThunkId m, MonadVar m)

instance (MonadBasicThunk m, MonadCatch m)
  => MonadThunk (NThunkF m v) (ThunkT m) v where
  thunk = ThunkT . buildThunk . unThunkT
  thunkId (Thunk n _ _) = n
  queryM t n k = ThunkT $ queryThunk t (unThunkT n) (unThunkT . k)
  force t k = ThunkT $ forceThunk t (unThunkT . k)
  forceEff t k = ThunkT $ forceEffects t (unThunkT . k)
  further t f = thunk $ f $ force t pure

buildThunk :: MonadBasicThunk m => m v -> m (NThunkF m v)
buildThunk action = do
  freshThunkId <- freshId
  Thunk freshThunkId <$> newVar False <*> newVar (Deferred $ withRootId freshThunkId action)

queryThunk :: MonadVar m => NThunkF m v -> m a -> (v -> m a) -> m a
queryThunk (Thunk _ active ref) n k = do
  nowActive <- atomicModifyVar active (True, )
  if nowActive
    then n
    else do
      eres <- readVar ref
      res  <- case eres of
        Computed v -> k v
        _          -> n
      _ <- atomicModifyVar active (False, )
      return res

forceThunk
  :: forall m v a
   . (MonadVar m, MonadThrow m, MonadCatch m, Show (ThunkId m))
  => NThunkF m v
  -> (v -> m a)
  -> m a
forceThunk (Thunk n active ref) k = do
  eres <- readVar ref
  case eres of
    Computed v      -> k v
    Deferred action -> do
      nowActive <- atomicModifyVar active (True, )
      if nowActive
        then throwM $ ThunkLoop $ show n
        else do
          traceM $ "Forcing " ++ show n
          v <- catch action $ \(e :: SomeException) -> do
            _ <- atomicModifyVar active (False, )
            throwM e
          _ <- atomicModifyVar active (False, )
          writeVar ref (Computed v)
          k v

forceEffects :: MonadVar m => NThunkF m v -> (v -> m r) -> m r
forceEffects (Thunk _ active ref) k = do
  nowActive <- atomicModifyVar active (True, )
  if nowActive
    then return $ error "Loop detected"
    else do
      eres <- readVar ref
      case eres of
        Computed v      -> k v
        Deferred action -> do
          v <- action
          writeVar ref (Computed v)
          _ <- atomicModifyVar active (False, )
          k v

{-
--[ryantrinkle] I'm worried about what impact this will have on the way withRootId works
furtherThunk :: MonadVar m => NThunkF m v -> (m v -> m v) -> m (NThunkF m v)
furtherThunk t@(Thunk _ _ ref) k = do
  _ <- atomicModifyVar ref $ \x -> case x of
    Computed _ -> (x, x)
    Deferred d -> (Deferred (k d), x)
  return t
-}
