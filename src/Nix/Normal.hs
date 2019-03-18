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
import           Data.Fix
import           Data.Set
import           Nix.Cited
import           Nix.Frames
import           Nix.String
import           Nix.Thunk
import           Nix.Value
import           Nix.Utils

newtype NormalLoop t f m = NormalLoop (NValue t f m)
    deriving Show

instance MonadDataErrorContext t f m => Exception (NormalLoop t f m)

-- | Normalize the value as much as possible, leaving only detected cycles.
normalize
  :: forall e t m f
   . ( Framed e m
     , MonadThunk t m (NValue t f m)
     , MonadDataErrorContext t f m
     , Ord (ThunkId m)
     )
  => (forall r . t -> (NValue t f m -> m r) -> m r)
  -> NValue t f m
  -> m (NValue t f m)
normalize f = run . iterNValueM run go (fmap Free . sequenceNValue' run)
 where
  start = 0 :: Int
  table = mempty

  run :: ReaderT Int (StateT (Set (ThunkId m)) m) r -> m r
  run = (`evalStateT` table) . (`runReaderT` start)

  go
    :: t
    -> (  NValue t f m
       -> ReaderT Int (StateT (Set (ThunkId m)) m) (NValue t f m)
       )
    -> ReaderT Int (StateT (Set (ThunkId m)) m) (NValue t f m)
  go t k = do
    b <- seen t
    if b
      then return $ Pure t
      else do
        i <- ask
        when (i > 2000)
          $ error "Exceeded maximum normalization depth of 2000 levels"
        lifted (lifted (f t)) $ local succ . k

  seen t = do
    let tid = thunkId t
    lift $ do
      res <- gets (member tid)
      unless res $ modify (insert tid)
      return res

stubCycles
  :: forall t f m
  . ( Applicative f
    , Functor m
    , HasCitations m (NValue t f m) t
    , HasCitations1 m (NValue t f m) f
    )
  => NValue t f m -> NValueNF t f m
stubCycles = freeToFix $ \t -> Fix
  $ NValue
  $ Prelude.foldr (addProvenance1 @m @(NValue t f m)) cyc
  $ reverse
  $ citations @m @(NValue t f m) t
  where
  Fix (NValue cyc) =
    nvStrNF (principledMakeNixStringWithoutContext "<CYCLE>")

normalForm
  :: ( Framed e m
     , MonadThunk t m (NValue t f m)
     , MonadDataErrorContext t f m
     , HasCitations m (NValue t f m) t
     , HasCitations1 m (NValue t f m) f
     , Ord (ThunkId m)
     )
  => NValue t f m
  -> m (NValueNF t f m)
normalForm = fmap stubCycles . normalize force

normalForm_
  :: ( Framed e m
     , MonadThunk t m (NValue t f m)
     , MonadDataErrorContext t f m
     , Ord (ThunkId m)
     )
  => NValue t f m
  -> m ()
normalForm_ = void <$> normalize forceEff

removeEffects
  :: (MonadThunk t m (NValue t f m), MonadDataContext f m)
  => NValue t f m
  -> m (NValueNF t f m)
removeEffects = nValueToNFM id (flip queryM (pure opaque))

opaque
  :: (MonadThunk t m (NValue t f m), MonadDataContext f m) => NValueNF t f m
opaque = nvStrNF $ principledMakeNixStringWithoutContext "<thunk>"

dethunk
  :: (MonadThunk t m (NValue t f m), MonadDataContext f m)
  => t
  -> m (NValueNF t f m)
dethunk t = queryM t (pure opaque) removeEffects
