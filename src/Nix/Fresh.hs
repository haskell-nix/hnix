{-# language CPP #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}

{-# options_ghc -Wno-orphans #-}


module Nix.Fresh where

import           Nix.Prelude
import           Control.Monad.Base   ( MonadBase(..) )
import           Control.Monad.Catch  ( MonadCatch
                                      , MonadMask
                                      , MonadThrow
                                      )
import           Control.Monad.Fix    ( MonadFix )
import           Control.Monad.Ref    ( MonadAtomicRef(..)
                                      , MonadRef(Ref)
                                      )

import           Nix.Thunk

newtype FreshIdT i m a = FreshIdT (ReaderT (Ref m i) m a)
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
  ( MonadAtomicRef m
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
    atomicModifyRef v (\i -> (succ i, i))

runFreshIdT :: Functor m => FreshIdT i m a -> Ref m i -> m a
runFreshIdT = runReaderT . coerce
