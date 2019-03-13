{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Nix.Normal where

import           Control.Monad
import           Control.Monad.Free
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State
import           Data.Functor.Compose
import           Data.Set
import           Nix.Frames
import           Nix.String
import           Nix.Thunk
import           Nix.Value

newtype NormalLoop m = NormalLoop (NValue m)
    deriving Show

instance (MonadDataContext m, Typeable m) => Exception (NormalLoop m)

normalForm'
    :: forall e m.
    (Framed e m,
     Typeable m,
     MonadThunk (NValue m) (NThunk m) m,
     MonadDataContext m)
    => (forall r. NThunk m -> (NValue m -> m r) -> m r)
    -> NValue m -> m (NValueNF m)
normalForm' f = run . nValueToNFM run go
  where
    start = 0 :: Int
    table = mempty

    run :: ReaderT Int (StateT (Set Int) m) r -> m r
    run = (`evalStateT` table) . (`runReaderT` start)

    go :: NThunk m
       -> (NValue m -> ReaderT Int (StateT (Set Int) m) (NValueNF m))
       -> ReaderT Int (StateT (Set Int) m) (NValueNF m)
    go t k = do
        i <- ask
        when (i > 2000) $
            error "Exceeded maximum normalization depth of 2000 levels"
        s <- lift get
        (res, s') <- lift $ lift $ f t $ \v ->
            (`runStateT` s) . (`runReaderT` i) $ local succ $ do
                b <- seen t
                if b
                    then return $ NValueNF $ Pure v
                    else k v
        lift $ put s'
        return res

    seen t = do
        let tid = thunkId t
        lift $ do
            res <- gets (member tid)
            unless res $ modify (insert tid)
            return res

normalForm
    :: forall e m.
    (Framed e m,
     Typeable m,
     MonadThunk (NValue m) (NThunk m) m,
     MonadDataContext m)
    => NValue m -> m (NValueNF m)
normalForm = normalForm' force

normalForm_
    :: forall e m.
    (Framed e m,
     Typeable m,
     MonadThunk (NValue m) (NThunk m) m,
     MonadDataContext m)
    => NValue m -> m ()
normalForm_ = void . normalForm' forceEff

removeEffects :: forall m. (MonadThunk (NValue m) (NThunk m) m, MonadDataContext m)
              => NValue m -> NValueNF m
removeEffects = nValueToNF (flip query opaque)

removeEffectsM :: forall m. (MonadThunk (NValue m) (NThunk m) m, MonadDataContext m)
               => NValue m -> m (NValueNF m)
removeEffectsM = nValueToNFM id (flip queryM (pure opaque))

opaque :: forall m. (MonadThunk (NValue m) (NThunk m) m, MonadDataContext m)
       => NValueNF m
opaque = NValueNF $ Free $ Compose $ pure $ NVStrF @(NValue m) $
    principledMakeNixStringWithoutContext "<thunk>"
