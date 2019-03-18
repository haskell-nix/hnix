{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Nix.Thunk.Standard where

import           Control.Comonad                ( Comonad )
import           Control.Comonad.Env            ( ComonadEnv )
import           Control.Monad.Catch     hiding ( catchJust )
import           Control.Monad.Reader
import           Control.Monad.Ref
import           Data.Typeable
import           GHC.Generics
import           Nix.Cited
import           Nix.Cited.Basic
import           Nix.Exec
import           Nix.Fresh
import           Nix.Fresh.Basic
import           Nix.Options
import           Nix.Thunk
import           Nix.Thunk.Basic
import           Nix.Value
import           Nix.Var

newtype StdThunk (u :: (* -> *) -> * -> *) (m :: * -> *) = StdThunk
  { _stdThunk :: StdCited u m (NThunkF (StdLazy u m) (StdValue u m)) }

newtype StdCited u m a = StdCited
    { _stdCited :: Cited (StdThunk u m) (StdCited u m) (StdLazy u m) a }
    deriving
        ( Generic
        , Typeable
        , Functor
        , Applicative
        , Foldable
        , Traversable
        , Comonad
        , ComonadEnv [Provenance (StdThunk u m) (StdLazy u m) (StdValue u m)]
        )

type StdValue u m   = NValue   (StdThunk u m) (StdCited u m) (StdLazy u m)
type StdValueNF u m = NValueNF (StdThunk u m) (StdCited u m) (StdLazy u m)
-- type StdIdT m     = FreshIdT Int m

type StdLazy u m  = Lazy (StdThunk u m) (StdCited u m) (u m)

instance Show (StdThunk u m) where
  show _ = "<thunk>"          -- jww (2019-03-15): NYI

type MonadStdThunk m
    = ( MonadVar m
      , MonadCatch m
      , MonadThrow m
      , Typeable m
      , MonadAtomicRef m
      )

instance ( MonadStdThunk (u m)
         , MonadThunkId (u m)
         , MonadTrans u
         , Typeable u
         , Typeable m
         )
  => MonadThunk (StdThunk u m) (StdLazy u m) (StdValue u m) where
  thunk        = fmap (StdThunk . StdCited) . thunk
  thunkId      = thunkId . _stdCited . _stdThunk
  query x b f  = query (_stdCited (_stdThunk x)) b f
  queryM x b f = queryM (_stdCited (_stdThunk x)) b f
  force        = force . _stdCited . _stdThunk
  forceEff     = forceEff . _stdCited . _stdThunk
  wrapValue    = StdThunk . StdCited . wrapValue
  getValue     = getValue . _stdCited . _stdThunk

instance HasCitations1 (StdThunk u m) (StdLazy u m) (StdValue u m) (StdCited u m) where
  citations1 (StdCited c) = citations1 c
  addProvenance1 x (StdCited c) = StdCited (addProvenance1 x c)

runStdLazyM :: (MonadVar m, MonadIO m, MonadIO (u m))
            => Options -> (u m a -> m a) -> StdLazy u m a -> m a
runStdLazyM opts run action = run $ runLazyM opts action

type StandardThunk m   = StdThunk StdIdT m
type StandardValue m   = StdValue StdIdT m
type StandardValueNF m = StdValueNF StdIdT m
type StandardT m       = StdLazy StdIdT m

runStandard :: (MonadVar m, MonadIO m)
            => Options -> StdLazy StdIdT m a -> m a
runStandard opts action = do
  i <- newVar (1 :: Int)
  runStdLazyM opts (runFreshIdT i) action

runStandardIO :: Options -> StdLazy StdIdT IO a -> IO a
runStandardIO = runStandard
