{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Nix.Thunk where

import Control.Exception (Exception)
import Data.Typeable (Typeable)

class Monad m => MonadThunk t m v | t -> m, t -> v where
    thunk :: m v -> m t
    thunkId :: t -> Int
    query :: t -> r -> (v -> r) -> r
    queryM :: t -> m r -> (v -> m r) -> m r
    force :: t -> (v -> m r) -> m r
    forceEff :: t -> (v -> m ()) -> m ()
    wrapValue :: v -> t
    getValue :: t -> Maybe v

newtype ThunkLoop = ThunkLoop Int
    deriving (Show, Typeable)

instance Exception ThunkLoop
