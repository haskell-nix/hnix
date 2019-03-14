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

newtype NormalLoop f g m = NormalLoop (NValue f g m)
    deriving Show

instance(Typeable m, Typeable f, Typeable g, MonadDataContext f m)
  => Exception (NormalLoop f g m)

normalForm'
    :: forall e t m f g.
    (Framed e m,
     Typeable m,
     IsNThunk t f g m)
    => (forall r. t -> (NValue f g m -> m r) -> m r)
    -> NValue f g m -> m (NValueNF f g m)
normalForm' f = run . nValueToNFM run go
  where
    start = 0 :: Int
    table = mempty

    run :: ReaderT Int (StateT (Set Int) m) r -> m r
    run = (`evalStateT` table) . (`runReaderT` start)

    go :: NThunk f g m
       -> (NValue f g m -> ReaderT Int (StateT (Set Int) m) (NValueNF f g m))
       -> ReaderT Int (StateT (Set Int) m) (NValueNF f g m)
    go t k = do
        i <- ask
        when (i > 2000) $
            error "Exceeded maximum normalization depth of 2000 levels"
        s <- lift get
        (res, s') <- lift $ lift $ f t $ \v ->
            (`runStateT` s) . (`runReaderT` i) $ local succ $ do
                b <- seen t
                if b
                    then return $ Pure v
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
    :: forall e t m f g. (Framed e m, Typeable m, IsNThunk t f g m)
    => NValue f g m -> m (NValueNF f g m)
normalForm = normalForm' @e @t @m force

normalForm_
    :: forall e t m f g. (Framed e m, Typeable m, IsNThunk t f g m)
    => NValue f g m -> m ()
normalForm_ = void . normalForm' @e @t @m forceEff

removeEffects
    :: forall m f g. (MonadThunk (NValue f g m) (NThunk f g m) m,
                MonadDataContext f m)
    => NValue f g m -> NValueNF f g m
removeEffects = nValueToNF (flip query opaque)

removeEffectsM
    :: forall m f g. (MonadThunk (NValue f g m) (NThunk f g m) m,
                MonadDataContext f m)
    => NValue f g m -> m (NValueNF f g m)
removeEffectsM = nValueToNFM id (flip queryM (pure opaque))

opaque :: forall m f g. (MonadThunk (NValue f g m) (NThunk f g m) m,
                   MonadDataContext f m)
       => NValueNF f g m
opaque = Free $ Compose $ pure $ NVStrF @(NValue f g m) $
    principledMakeNixStringWithoutContext "<thunk>"

dethunk :: (MonadThunk (NValue f g m) (NThunk f g m) m,
           MonadDataContext f m)
        => NThunk f g m -> m (NValueNF f g m)
dethunk t = queryM t (pure opaque) removeEffectsM
