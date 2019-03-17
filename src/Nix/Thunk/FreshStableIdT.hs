{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Nix.Thunk.FreshStableIdT (FreshStableIdT, runFreshStableIdT) where

import Nix.Thunk
import Nix.Thunk.StableId
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Ref
import Control.Monad.Catch
import Control.Applicative

newtype FreshStableIdT m a = FreshStableIdT (ReaderT StableId (StateT Int m) a)
  deriving (Functor, Applicative, Monad, MonadRef, MonadAtomicRef, MonadCatch, MonadThrow, MonadIO, MonadFix, MonadPlus, Alternative)

instance MonadTrans FreshStableIdT where
  lift = FreshStableIdT . lift . lift

runFreshStableIdT :: Monad m => StableId -> FreshStableIdT m a -> m a
runFreshStableIdT root (FreshStableIdT a) = evalStateT (runReaderT a root) 0

instance Monad m => MonadThunkId (FreshStableIdT m) where
  type ThunkId (FreshStableIdT m) = StableId
  freshId = FreshStableIdT $ do
    root <- ask
    n <- get
    put $ succ n
    return $ cons n root
