{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}

module Nix.Thunk where

import           Control.Exception              ( Exception )
import           Control.Monad.Trans.Class      ( MonadTrans(..) )
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State
import           Control.Monad.Trans.Writer
import           Data.Typeable                  ( Typeable )

class ( Monad m
      , Eq (ThunkId m)
      , Ord (ThunkId m)
      , Show (ThunkId m)
      , Typeable (ThunkId m)
      )
      => MonadThunkId m where
  type ThunkId m :: *
  freshId :: m (ThunkId m)
  default freshId
      :: ( MonadThunkId m'
        , MonadTrans t
        , m ~ t m'
        , ThunkId m ~ ThunkId m'
        )
      => m (ThunkId m)
  freshId = lift freshId

instance MonadThunkId m => MonadThunkId (ReaderT r m) where
  type ThunkId (ReaderT r m) = ThunkId m
instance (Monoid w, MonadThunkId m) => MonadThunkId (WriterT w m) where
  type ThunkId (WriterT w m) = ThunkId m
instance MonadThunkId m => MonadThunkId (ExceptT e m) where
  type ThunkId (ExceptT e m) = ThunkId m
instance MonadThunkId m => MonadThunkId (StateT s m) where
  type ThunkId (StateT s m) = ThunkId m

class MonadThunkId m => MonadThunk t m a | t -> m, t -> a where
  thunk :: m a -> m t

  -- | Return an identifier for the thunk unless it is a pure value (i.e.,
  --   strictly an encapsulation of some 'a' without any additional
  --   structure). For pure values represented as thunks, returns mempty.
  thunkId :: t -> ThunkId m

  queryM :: t -> m r -> (a -> m r) -> m r
  force :: t -> (a -> m r) -> m r
  forceEff :: t -> (a -> m r) -> m r

  -- | Modify the action to be performed by the thunk. For some implicits
  --   this modifies the thunk, for others it may create a new thunk.
  further :: t -> (m a -> m a) -> m t

newtype ThunkLoop = ThunkLoop String -- contains rendering of ThunkId
  deriving Typeable

instance Show ThunkLoop where
  show (ThunkLoop i) = "ThunkLoop " <> i

instance Exception ThunkLoop
