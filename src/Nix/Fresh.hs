{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}


module Nix.Fresh where

import           Control.Applicative        ( Alternative )
import           Control.Monad.Base   ( MonadBase(..) )
import           Control.Monad.Catch  ( MonadCatch
                              , MonadMask
                              , MonadThrow
                              )
import           Control.Monad.Except
                              ( MonadFix
                              , MonadIO
                              , MonadPlus
                              , MonadTrans(..)
                              )
#if !MIN_VERSION_base(4,13,0)
import           Control.Monad.Fail
#endif
import           Control.Monad.Reader ( ReaderT(..)
                              , MonadReader(ask)
                              )
import           Control.Monad.Ref    ( MonadAtomicRef(..)
                              , MonadRef(writeRef, readRef)
                              )
import           Control.Monad.ST     ( ST )
import           Data.Typeable     ( Typeable )

import           Nix.Var


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

freshId :: (Monad m, MonadAtomicRef m, Enum i) => FreshIdT i m i
freshId = FreshIdT $ do
  v <- ask
  atomicModifyVar v (\i -> (succ i, i))

runFreshIdT :: Functor m => Var m i -> FreshIdT i m a -> m a
runFreshIdT i m = runReaderT (unFreshIdT m) i

-- Orphan instance needed by Infer.hs and Lint.hs

-- Since there's no forking, it's automatically atomic.
--  2021-02-09: NOTE: Submitted upstream: https://github.com/mainland/ref-tf/pull/4
instance MonadAtomicRef (ST s) where
  atomicModifyRef r f = do
    v <- readRef r
    let (a, b) = f v
    writeRef r a
    pure b
  atomicModifyRef' r f = do
    v <- readRef r
    let (a, b) = f v
    writeRef r $! a
    pure b
