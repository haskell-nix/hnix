{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}


module Nix.Fresh where

import           Control.Monad.Base   ( MonadBase(..) )
import           Control.Monad.Catch  ( MonadCatch
                                      , MonadMask
                                      , MonadThrow
                                      )
import           Control.Monad.Except ( MonadFix )
import           Control.Monad.Ref    ( MonadAtomicRef(..)
                                      , MonadRef()
                                      )

import           Nix.Var
import           Nix.Thunk


newtype FreshIdT i m a = FreshIdT { unFreshIdT :: ReaderT (Var m i) m a }
  deriving
    ( Functor
    , Applicative
    , Alternative
    , Monad
    , MonadFail
    , MonadPlus
    , MonadFix
    , MonadRef
    , MonadAtomicRef
    , MonadIO
    , MonadCatch
    , MonadThrow
    , MonadMask
    )

instance MonadTrans (FreshIdT i) where
  lift = FreshIdT . lift

instance MonadBase b m => MonadBase b (FreshIdT i m) where
  liftBase = FreshIdT . liftBase

instance
  ( MonadVar m
  , Eq i
  , Ord i
  , Show i
  , Enum i
  , Typeable i
  )
  => MonadThunkId (FreshIdT i m)
 where
  type ThunkId (FreshIdT i m) = i
  freshId = FreshIdT $ do
    v <- ask
    atomicModifyVar v (\i -> (succ i, i))

runFreshIdT :: Functor m => Var m i -> FreshIdT i m a -> m a
runFreshIdT i m = runReaderT (unFreshIdT m) i
