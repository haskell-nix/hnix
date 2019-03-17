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

import           Control.Comonad                ( Comonad )
import           Control.Comonad.Env            ( ComonadEnv )
import           Control.Monad.Catch     hiding ( catchJust )
import           Control.Monad.Reader
import           Data.Fix
import           GHC.Generics
import           Nix.Cited
import           Nix.Convert
import           Nix.Effects
import           Nix.Eval                      as Eval
import           Nix.Exec
import           Nix.Expr
import           Nix.Frames
import           Nix.Fresh
import           Nix.Options
import           Nix.Render
import           Nix.Thunk
import           Nix.Thunk.Basic
import           Nix.Utils
import           Nix.Value
import           Nix.Var                        ( MonadVar
                                                , newVar
                                                )

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

type StdIdT m = FreshIdT Int m

type StdLazy m = Lazy (StdThunk m) (StdCited m) (StdIdT m)

type MonadStdThunk m = (MonadVar m, MonadCatch m, MonadThrow m, Typeable m)

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

        fmap (StdThunk . StdCited . NCited ps) . thunk $ mv
      else fmap (StdThunk . StdCited . NCited []) . thunk $ mv

  thunkId (StdThunk (StdCited (NCited _ t))) = thunkId t

  query (StdThunk (StdCited (NCited _ t))) = query t
  queryM (StdThunk (StdCited (NCited _ t))) = queryM t

-- The ThunkLoop exception is thrown as an exception with MonadThrow,
-- which does not capture the current stack frame information to provide
-- it in a NixException, so we catch and re-throw it here using
-- 'throwError' from Frames.hs.
  force (StdThunk (StdCited (NCited ps t))) f = catch go
                                                      (throwError @ThunkLoop)
   where
    go = case ps of
      [] -> force t f
      Provenance scope e@(Compose (Ann s _)) : _ ->
          -- r <- liftWith $ \run -> do
          --     withFrame Info (ForcingExpr scope (wrapExprLoc s e))
          --         (run (force t f))
          -- restoreT $ return r
        withFrame Info (ForcingExpr scope (wrapExprLoc s e)) (force t f)

  forceEff (StdThunk (StdCited (NCited ps t))) f = catch
    go
    (throwError @ThunkLoop)
   where
    go = case ps of
      [] -> forceEff t f
      Provenance scope e@(Compose (Ann s _)) : _ -> do
          -- r <- liftWith $ \run -> do
          --     withFrame Info (ForcingExpr scope (wrapExprLoc s e))
          --         (run (forceEff t f))
          -- restoreT $ return r
        withFrame Info (ForcingExpr scope (wrapExprLoc s e)) (forceEff t f)

  wrapValue = StdThunk . StdCited . NCited [] . wrapValue
  getValue (StdThunk (StdCited (NCited _ v))) = getValue v

instance ( MonadStdThunk m
         , ToValue a (StdLazy m) (StdValue m)
         )
         => ToValue a (StdLazy m) (StdThunk m) where
  toValue = fmap wrapValue . toValue

instance MonadStdThunk m
  => ToValue (StdThunk m) (StdLazy m) (StdValue m) where
  toValue = force ?? pure

instance ( MonadStdThunk m
         , FromValue a (StdLazy m) (StdValue m)
         )
         => FromValue a (StdLazy m) (StdThunk m) where
  fromValueMay = force ?? fromValueMay
  fromValue    = force ?? fromValue

instance MonadStdThunk m
  => FromValue (StdThunk m) (StdLazy m) (StdValue m) where
  fromValueMay = pure . Just . wrapValue
  fromValue    = pure . wrapValue

instance ( MonadStdThunk m
         , ToNix a (StdLazy m) (StdValue m)
         )
         => ToNix a (StdLazy m) (StdThunk m) where
  toNix = fmap wrapValue . toNix

instance MonadStdThunk m
  => ToNix (StdThunk m) (StdLazy m) (StdValue m) where
  toNix = force ?? pure

instance ( MonadStdThunk m
         , FromNix a (StdLazy m) (StdValue m)
         )
         => FromNix a (StdLazy m) (StdThunk m) where
  fromNixMay = force ?? fromNixMay
  fromNix    = force ?? fromNix

instance MonadStdThunk m
  => FromNix (StdThunk m) (StdLazy m) (StdValue m) where
  fromNixMay = pure . Just . wrapValue
  fromNix    = pure . wrapValue

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
    i <- FreshIdT ask
    p <- lift $ importPath @t @f @m path
    return $ liftNValue (runFreshIdT i) p
  pathToDefaultNix = lift . pathToDefaultNix @t @f @m
  derivationStrict v = do
    i <- FreshIdT ask
    p <- lift $ derivationStrict @t @f @m (unliftNValue (runFreshIdT i) v)
    return $ liftNValue (runFreshIdT i) p
  traceEffect = lift . traceEffect @t @f @m

instance HasCitations1 (StdThunk m) (StdCited m) (StdLazy m) where
  citations1 (StdCited c) = citations c
  addProvenance1 x (StdCited c) = StdCited (addProvenance x c)

runStdLazyM :: (MonadVar m, MonadIO m) => Options -> StdLazy m a -> m a
runStdLazyM opts action = do
  i <- newVar (1 :: Int)
  runFreshIdT i $ runLazyM opts action
