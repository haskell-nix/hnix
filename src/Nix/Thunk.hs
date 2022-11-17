{-# language DefaultSignatures #-}
{-# language FunctionalDependencies #-}
{-# language TypeFamilies #-}
{-# language TypeOperators #-}

module Nix.Thunk where

import           Nix.Prelude
import           Control.Monad.Trans.Writer ( WriterT )
import qualified Text.Show


-- ** @class MonadThunkId@ & @instances@

class
  ( Monad m
  , Eq (ThunkId m)
  , Ord (ThunkId m)
  , Show (ThunkId m)
  , Typeable (ThunkId m)
  )
  => MonadThunkId m
 where
  type ThunkId m :: Type

  freshId :: m (ThunkId m)
  default freshId
    :: ( MonadThunkId m'
      , MonadTrans t
      , m ~ t m'
      , ThunkId m ~ ThunkId m'
      )
    => m (ThunkId m)
  freshId = lift freshId


-- *** Instances

instance
  MonadThunkId m
  => MonadThunkId (ReaderT r m)
 where
  type ThunkId (ReaderT r m) = ThunkId m

instance
  ( Monoid w
  , MonadThunkId m
  )
  => MonadThunkId (WriterT w m)
 where
  type ThunkId (WriterT w m) = ThunkId m

instance
  MonadThunkId m
  => MonadThunkId (ExceptT e m)
 where
  type ThunkId (ExceptT e m) = ThunkId m

instance
  MonadThunkId m
  => MonadThunkId (StateT s m)
 where
  type ThunkId (StateT s m) = ThunkId m


-- ** @class MonadThunk@

class
  MonadThunkId m
  => MonadThunk t m a | t -> m, t -> a
 where

  -- | Return thunk ID.
  thunkId  :: t -> ThunkId m

  -- | Create new thunk
  thunk    :: m a -> m t

  -- | Non-blocking query.
  --   If thunk got computed
  --   then return its value
  --   otherwise return default value (1st arg).
  query   :: m a -> t -> m a
  force    :: t -> m a
  forceEff :: t -> m a

  -- | Modify the action to be performed by the thunk. For some implicits
  --   this modifies the thunk, for others it may create a new thunk.
  further  :: t -> m t


-- ** @class MonadThunk@

-- | Class of Kleisli functors for easiness of customized implementation developlemnt.
class
  MonadThunkF t m a | t -> m, t -> a
 where
  queryF   :: (a   -> m r) -> m r -> t -> m r
  forceF    :: (a   -> m r) -> t   -> m r
  forceEffF :: (a   -> m r) -> t   -> m r
  furtherF  :: (m a -> m a) -> t   -> m t


-- ** @newtype ThunkLoop@

newtype ThunkLoop = ThunkLoop Text -- contains rendering of ThunkId
  deriving Typeable

instance Show ThunkLoop where
  show (ThunkLoop i) = toString $ "ThunkLoop " <> i

instance Exception ThunkLoop

-- ** Utils

thunkStubText :: Text
thunkStubText = "<thunk>"
