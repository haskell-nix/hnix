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
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Nix.Thunk.Standard where

import           Control.Applicative
import           Control.Comonad                ( Comonad )
import           Control.Comonad.Env            ( ComonadEnv )
import           Control.Monad.Catch     hiding ( catchJust )
import           Control.Monad.Reader
import           Data.Fix
import           GHC.Generics
import           Nix.Cited
import           Nix.Effects
import           Nix.Eval                      as Eval
import           Nix.Exec
import           Nix.Expr
import           Nix.Frames
import           Nix.Options
import           Nix.Render
import           Nix.Thunk
import           Nix.Thunk.Separate
import qualified Nix.Thunk.StableId            as StableId
import           Nix.Thunk.FreshStableIdT
import           Nix.Utils
import           Nix.Value
import           Control.Monad.Ref

newtype StdCited m a = StdCited
    { _stdCited :: NCited (StdThunk m) (StdCited m) (StdLazy m) a }
    deriving
        ( Generic
        , Typeable
        , Functor
        , Applicative
        , Foldable
        , Traversable
        , Comonad
        , ComonadEnv [Provenance (StdThunk m) (StdCited m) (StdLazy m)]
        )

newtype StdThunk m = StdThunk
    { _stdThunk :: StdCited m (NThunkF (StdLazy m) (StdValue m)) }

type StdValue m = NValue (StdThunk m) (StdCited m) (StdLazy m)
type StdValueNF m = NValueNF (StdThunk m) (StdCited m) (StdLazy m)

newtype StdIdT m a = StdIdT { unStdIdT :: SeparateThunkT (StdLazy m) (StdValue m) (FreshStableIdT m) a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadThunkId
    , MonadCatch
    , MonadThrow
    , MonadIO
    , MonadPlus
    , Alternative
    , MonadFix
    )

type StdLazy m = Lazy (StdThunk m) (StdCited m) (StdIdT m)

type MonadStdThunk m = (MonadAtomicRef m, MonadCatch m, MonadThrow m, Typeable m)

instance MonadTrans StdIdT where
  lift = StdIdT . lift . lift

instance Monad m => Forcer (StdLazy m) (StdValue m) (FreshStableIdT m) where
  liftSeparateThunkT = lift . StdIdT

instance MonadStdThunk m
  => MonadThunk (StdThunk m) (StdLazy m) (StdValue m) where
  thunk mv = do
    opts :: Options <- asks (view hasLens)

    if thunks opts
      then do
        frames :: Frames <- asks (view hasLens)

        -- Gather the current evaluation context at the time of thunk
        -- creation, and record it along with the thunk.
        let go (fromException ->
                    Just (EvaluatingExpr scope
                             (Fix (Compose (Ann s e))))) =
                let e' = Compose (Ann s (Nothing <$ e))
                in [Provenance scope e']
            go _ = []
            ps = concatMap (go . frame) frames

        lift $ StdIdT $ fmap (StdThunk . StdCited . NCited ps) $ buildThunk mv
      else lift $ StdIdT $ fmap (StdThunk . StdCited . NCited []) $ buildThunk mv

  thunkId (StdThunk (StdCited (NCited _ t))) = separateThunkId t

  query (StdThunk (StdCited (NCited _ t))) = queryValue t
  queryM (StdThunk (StdCited (NCited _ t))) = queryThunk t

  -- | The ThunkLoop exception is thrown as an exception with MonadThrow,
  --   which does not capture the current stack frame information to provide
  --   it in a NixException, so we catch and re-throw it here using
  --   'throwError' from Frames.hs.
  force (StdThunk (StdCited (NCited ps t))) f =
    catch go (throwError @ThunkLoop)
   where
    go = case ps of
      [] -> forceThunk t f
      Provenance scope e@(Compose (Ann s _)) : _ ->
        withFrame Info (ForcingExpr scope (wrapExprLoc s e)) (forceThunk t f)

  forceEff (StdThunk (StdCited (NCited ps t))) f = catch
    go
    (throwError @ThunkLoop)
   where
    go = case ps of
      [] -> forceEffects t f
      Provenance scope e@(Compose (Ann s _)) : _ ->
        withFrame Info (ForcingExpr scope (wrapExprLoc s e)) (forceEffects t f)

  wrapValue = StdThunk . StdCited . NCited [] . valueRef
  getValue (StdThunk (StdCited (NCited _ v))) = thunkValue v

instance Show (StdThunk m) where
  show _ = "<thunk>"          -- jww (2019-03-15): NYI

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
    i <- freshId
    c <- StdIdT askThunkCache
    p <- lift $ importPath @t @f @m path
    return $ liftNValue (runFreshStableIdT i . runSeparateThunkT c . unStdIdT) p
  pathToDefaultNix = lift . pathToDefaultNix @t @f @m
  derivationStrict v = do
    i <- freshId
    c <- StdIdT askThunkCache
    p <- lift $ derivationStrict @t @f @m (unliftNValue (runFreshStableIdT i . runSeparateThunkT c . unStdIdT) v)
    return $ liftNValue (runFreshStableIdT i . runSeparateThunkT c . unStdIdT) p
  traceEffect = lift . traceEffect @t @f @m

instance HasCitations1 (StdThunk m) (StdCited m) (StdLazy m) where
  citations1 (StdCited c) = citations c
  addProvenance1 x (StdCited c) = StdCited (addProvenance x c)

runStdLazyM :: (MonadIO m, MonadRef m) => Options -> StdLazy m a -> m a
runStdLazyM opts action = do
  c <- newThunkCache
  runFreshStableIdT StableId.nil $ runSeparateThunkT c $ unStdIdT $ runLazyM opts action
