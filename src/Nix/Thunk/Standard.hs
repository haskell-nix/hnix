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
import           Nix.Effects
import           Nix.Exec
import           Nix.Fresh
import           Nix.Options
import           Nix.Render
import           Nix.Thunk
import           Nix.Thunk.Basic
import           Nix.Value
import           Nix.Var                        ( MonadVar
                                                , newVar
                                                )

newtype StdThunk m = StdThunk
  { _stdThunk :: StdCited m (NThunkF (StdLazy m) (StdValue m)) }

newtype StdCited m a = StdCited
    { _stdCited :: Cited (StdThunk m) (StdCited m) (StdLazy m) a }
    deriving
        ( Generic
        , Typeable
        , Functor
        , Applicative
        , Foldable
        , Traversable
        , Comonad
        , ComonadEnv [Provenance (StdThunk m) (StdLazy m) (StdValue m)]
        )

type StdValue m   = NValue   (StdThunk m) (StdCited m) (StdLazy m)
type StdValueNF m = NValueNF (StdThunk m) (StdCited m) (StdLazy m)
type StdIdT m     = FreshIdT Int m

type StdLazy m    = Lazy (StdThunk m) (StdCited m) (StdIdT m)

instance Show (StdThunk m) where
  show _ = "<thunk>"          -- jww (2019-03-15): NYI

type MonadStdThunk m
    = ( MonadVar m
      , MonadCatch m
      , MonadThrow m
      , Typeable m
      , MonadAtomicRef m
      )

instance MonadStdThunk m
  => MonadThunk (StdThunk m) (StdLazy m) (StdValue m) where
  thunk        = fmap (StdThunk . StdCited) . thunk
  thunkId      = thunkId . _stdCited . _stdThunk
  query x b f  = query (_stdCited (_stdThunk x)) b f
  queryM x b f = queryM (_stdCited (_stdThunk x)) b f
  force        = force . _stdCited . _stdThunk
  forceEff     = forceEff . _stdCited . _stdThunk
  wrapValue    = StdThunk . StdCited . wrapValue
  getValue     = getValue . _stdCited . _stdThunk

instance MonadFile m => MonadFile (StdIdT m)
instance MonadIntrospect m => MonadIntrospect (StdIdT m)
instance MonadStore m => MonadStore (StdIdT m) where
  addPath' = lift . addPath'
  toFile_' = (lift .) . toFile_'
instance MonadPutStr m => MonadPutStr (StdIdT m)
instance MonadHttp m => MonadHttp (StdIdT m)
instance MonadEnv m => MonadEnv (StdIdT m)
instance MonadInstantiate m => MonadInstantiate (StdIdT m)
instance MonadExec m => MonadExec (StdIdT m)

instance (MonadEffects t f m, MonadDataContext f m)
  => MonadEffects t f (StdIdT m) where
  makeAbsolutePath = lift . makeAbsolutePath @t @f @m
  findEnvPath      = lift . findEnvPath @t @f @m
  findPath         = (lift .) . findPath @t @f @m
  importPath path = do
    i <- FreshIdT ask
    p <- lift $ importPath @t @f @m path
    return $ liftNValue (runFreshIdT i) p
  pathToDefaultNix = lift . pathToDefaultNix @t @f @m
  derivationStrict v = do
    i <- FreshIdT ask
    p <- lift $ derivationStrict @t @f @m (unliftNValue (runFreshIdT i) v)
    return $ liftNValue (runFreshIdT i) p
  traceEffect = lift . traceEffect @t @f @m

instance HasCitations1 (StdThunk m) (StdLazy m) (StdValue m) (StdCited m) where
  citations1 (StdCited c) = citations1 c
  addProvenance1 x (StdCited c) = StdCited (addProvenance1 x c)

runStdLazyM :: (MonadVar m, MonadIO m) => Options -> StdLazy m a -> m a
runStdLazyM opts action = do
  i <- newVar (1 :: Int)
  runFreshIdT i $ runLazyM opts action
