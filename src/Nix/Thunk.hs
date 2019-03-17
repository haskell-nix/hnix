{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Nix.Thunk where

import           Control.Exception              ( Exception )
import           Control.Monad.Trans.Class      ( MonadTrans(..) )
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

class MonadThunkId m => MonadThunk t m v | t -> m, t -> v where
    thunk :: m v -> m t
    -- | Return an identifier for the thunk unless it is a pure value (i.e.,
    --   strictly an encapsulation of some 'v' without any additional
    --   structure). For pure values represented as thunks, returns Nothing.
    thunkId :: t -> Maybe (ThunkId m)
    query :: t -> r -> (v -> r) -> r
    queryM :: t -> m r -> (v -> m r) -> m r
    force :: t -> (v -> m r) -> m r
    forceEff :: t -> (v -> m r) -> m r
    wrapValue :: v -> t
    getValue :: t -> Maybe v

newtype ThunkLoop = ThunkLoop String -- contains rendering of ThunkId
    deriving Typeable

instance Show ThunkLoop where
  show (ThunkLoop i) = "ThunkLoop " ++ i

instance Exception ThunkLoop
