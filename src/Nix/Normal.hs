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
import           Data.Set
import           Nix.Frames
import           Nix.String
import           Nix.Thunk
import           Nix.Value

newtype NormalLoop t f m = NormalLoop (NValue t f m)
    deriving Show

instance MonadDataErrorContext t f m => Exception (NormalLoop t f m)

normalForm'
    :: forall e t m f.
    (Framed e m,
     MonadThunk t m (NValue t f m),
     MonadDataErrorContext t f m)
    => (forall r. t -> (NValue t f m -> m r) -> m r)
    -> NValue t f m
    -> m (NValueNF t f m)
normalForm' f = run . nValueToNFM run go
  where
    start = 0 :: Int
    table = mempty

    run :: ReaderT Int (StateT (Set Int) m) r -> m r
    run = (`evalStateT` table) . (`runReaderT` start)

    go :: t
       -> (NValue t f m -> ReaderT Int (StateT (Set Int) m) (NValueNF t f m))
       -> ReaderT Int (StateT (Set Int) m) (NValueNF t f m)
    go t k = do
        i <- ask
        when (i > 2000) $
            error "Exceeded maximum normalization depth of 2000 levels"
        s <- lift get
        (res, s') <- lift $ lift $ f t $ \v ->
            (`runStateT` s) . (`runReaderT` i) $ local succ $ do
                b <- seen t
                if b
                    then return $ Pure (undefined <$ v)
                    else k v
        lift $ put s'
        return res

    seen t = do
        let tid = thunkId t
        lift $ do
            res <- gets (member tid)
            unless res $ modify (insert tid)
            return res

normalForm :: (Framed e m,
              MonadThunk t m (NValue t f m),
              MonadDataErrorContext t f m)
           => NValue t f m -> m (NValueNF t f m)
normalForm = normalForm' force

normalForm_ :: (Framed e m,
               MonadThunk t m (NValue t f m),
               MonadDataErrorContext t f m)
            => NValue t f m -> m ()
normalForm_ = fmap void $ normalForm' $ \t k -> do
    forceEff t (void . k)
    -- This next return is safe, only because we never inspect this value, nor
    -- is anything returned to the user due to 'fmap void' above.
    return $ error "normalForm_: a value was expected"

removeEffects :: (MonadThunk t m (NValue t f m), MonadDataContext f m)
              => NValue t f m -> NValueNF t f m
removeEffects = nValueToNF (flip query opaque)

removeEffectsM :: (MonadThunk t m (NValue t f m), MonadDataContext f m)
               => NValue t f m -> m (NValueNF t f m)
removeEffectsM = nValueToNFM id (flip queryM (pure opaque))

opaque :: (MonadThunk t m (NValue t f m), MonadDataContext f m)
       => NValueNF t f m
opaque = nvStrNF $ principledMakeNixStringWithoutContext "<thunk>"

dethunk :: (MonadThunk t m (NValue t f m), MonadDataContext f m)
        => t -> m (NValueNF t f m)
dethunk t = queryM t (pure opaque) removeEffectsM
