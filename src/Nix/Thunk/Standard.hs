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

import           Control.Comonad (Comonad)
import           Control.Comonad.Env (ComonadEnv)
import           Control.Monad.Catch hiding (catchJust)
import           Control.Monad.Reader
import           Control.Monad.Trans.Control
import           Data.Fix
import           Data.Functor.Classes
import qualified Data.HashMap.Lazy as M
import           Data.Text (Text)
import           GHC.Generics
import           Nix.Cited
import           Nix.Convert
import           Nix.Effects
import           Nix.Eval as Eval
import           Nix.Exec
import           Nix.Expr
import           Nix.Frames
import           Nix.Fresh
import           Nix.Options
import           Nix.Render
import           Nix.Scope
import           Nix.String
import           Nix.Thunk
import           Nix.Thunk.Basic
import           Nix.Utils
import           Nix.Value
import           Nix.Var (MonadVar)

newtype StdCited m a = StdCited
    { _stdCited :: NCited (StdThunk m) (StdValue m) (StdLazy m) a }
    deriving
        ( Generic
        , Typeable
        , Functor
        , Applicative
        , Foldable
        , Traversable
        , Comonad
        , ComonadEnv [Provenance (StdThunk m) (StdValue m) (StdLazy m)]
        )

newtype StdThunk m = StdThunk
    { _stdThunk :: StdCited m (NThunkF (StdLazy m) (StdValue m)) }

type StdValue   m = NValue   (StdThunk m) (StdCited m) (StdLazy m)
type StdValueNF m = NValueNF (StdThunk m) (StdCited m) (StdLazy m)

type StdLazy m = Lazy (StdThunk m) (StdCited m) (FreshIdT Int m)

instance Show1 (StdLazy m) => Show1 (StdCited m) where
    liftShowsPrec f g n (StdCited c) = liftShowsPrec f g n c

instance ( MonadVar m
         , MonadCatch m
         , MonadThrow m
         , Typeable m
         )
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
            else
                fmap (StdThunk . StdCited . NCited []) . thunk $ mv

    thunkId = error "jww (2019-03-15): NYI"

    query = error "jww (2019-03-15): NYI"
    queryM = error "jww (2019-03-15): NYI"

    -- The ThunkLoop exception is thrown as an exception with MonadThrow,
    -- which does not capture the current stack frame information to provide
    -- it in a NixException, so we catch and re-throw it here using
    -- 'throwError' from Frames.hs.
    force (StdThunk (StdCited (NCited ps t))) f =
        catch go (throwError @ThunkLoop)
      where
        go = case ps of
            [] -> force t f
            Provenance scope e@(Compose (Ann s _)):_ ->
                -- r <- liftWith $ \run -> do
                --     withFrame Info (ForcingExpr scope (wrapExprLoc s e))
                --         (run (force t f))
                -- restoreT $ return r
                withFrame Info (ForcingExpr scope (wrapExprLoc s e))
                    (force t f)

    forceEff (StdThunk (StdCited (NCited ps t))) f =
        catch go (throwError @ThunkLoop)
      where
        go = case ps of
            [] -> forceEff t f
            Provenance scope e@(Compose (Ann s _)):_ -> do
                -- r <- liftWith $ \run -> do
                --     withFrame Info (ForcingExpr scope (wrapExprLoc s e))
                --         (run (forceEff t f))
                -- restoreT $ return r
                withFrame Info (ForcingExpr scope (wrapExprLoc s e))
                    (forceEff t f)

    wrapValue = StdThunk . StdCited . NCited [] . wrapValue
    getValue (StdThunk (StdCited (NCited _ v))) = getValue v

instance FromNix Bool (StdLazy IO) (StdThunk IO) where
instance FromNix [NixString] (StdLazy IO) (StdThunk IO) where
instance FromValue (M.HashMap Text (StdThunk m)) (StdLazy m) (StdThunk m) where
instance FromValue Bool (StdLazy IO) (StdThunk IO) where
instance FromValue NixString (StdLazy m) (StdThunk m) where
instance FromValue Path (StdLazy m) (StdThunk m) where
instance FromValue [NixString] (StdLazy IO) (StdThunk IO) where
instance FromValue [StdThunk m] (StdLazy m) (StdThunk m) where
instance ToNix (StdThunk m) (StdLazy m) (StdValue m) where
instance ToNix NixString (StdLazy m) (StdThunk m) where
instance ToNix [StdThunk m] (StdLazy m) (StdThunk m) where
instance ToValue () (StdLazy m) (StdThunk m) where
instance ToValue (StdThunk m) (StdLazy m) (StdValue m) where
instance ToValue Int (StdLazy m) (StdThunk m) where
instance ToValue NixString (StdLazy m) (StdThunk m) where
instance ToValue [StdThunk m] (StdLazy m) (StdThunk m) where

-- instance FromValue a (StdLazy m) (StdValue m) => FromValue a (StdLazy m) (StdThunk m) where
-- instance FromNix a (StdLazy m) (StdValue m) => FromNix a (StdLazy m) (StdThunk m) where

instance Show (StdThunk m) where
    show _ = "<thunk>"          -- jww (2019-03-15): NYI

deriving instance MonadReader e m => MonadReader e (FreshIdT Int m)
instance MonadFile m => MonadFile (FreshIdT Int m)
instance MonadIntrospect m => MonadIntrospect (FreshIdT Int m)
instance MonadStore m => MonadStore (FreshIdT Int m)
instance MonadPutStr m => MonadPutStr (FreshIdT Int m)
instance MonadHttp m => MonadHttp (FreshIdT Int m)
instance MonadEnv m => MonadEnv (FreshIdT Int m)
instance MonadInstantiate m => MonadInstantiate (FreshIdT Int m)
instance MonadExec m => MonadExec (FreshIdT Int m)

instance MonadEffects t f m => MonadEffects t f (FreshIdT Int m)

instance HasCitations1 (StdThunk m) (StdValue m) (StdLazy m) (StdCited m)

runStdLazyM :: MonadIO m => Options -> StdLazy m a -> m a
runStdLazyM opts = runFreshIdT (1 :: Int) . runLazyM opts
