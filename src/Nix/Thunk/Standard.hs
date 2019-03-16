{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Nix.Thunk.Standard where

import           Control.Monad.Catch hiding (catchJust)
import           Control.Monad.Reader
import           Control.Monad.Trans.Control
import           Data.Fix
import qualified Data.HashMap.Lazy as M
import           Data.Text (Text)
import           Nix.Cited
import           Nix.Convert
import           Nix.Eval as Eval
import           Nix.Exec
import           Nix.Expr
import           Nix.Frames
import           Nix.Fresh
import           Nix.Options
import           Nix.String
import           Nix.Thunk
import           Nix.Thunk.Basic
import           Nix.Utils
import           Nix.Value
import           Nix.Var (MonadVar)

newtype StdThunk m = StdThunk
    { _stdThunk ::
            NCited (StdThunk m) (StdValue m)
                   (FreshIdT Int m)
                   (NThunkF (FreshIdT Int m) (StdValue m)) }

newtype StdValue m = StdValue
    { _stdValue ::
            NValue (StdThunk m)
                   (NCited (StdThunk m) (StdValue m) (FreshIdT Int m))
                   (FreshIdT Int m) }

newtype StdValueNF m = StdValueNF
    { _stdValueNF ::
            NValueNF (StdThunk m)
                     (NCited (StdThunk m) (StdValue m) (FreshIdT Int m))
                     (FreshIdT Int m) }

type StdLazy m =
    Lazy (StdThunk m)
         (NCited (StdThunk m) (StdValue m) (FreshIdT Int m))
         (FreshIdT Int m)

instance (MonadNix e t f m, MonadVar m)
  => MonadThunk (StdThunk m) (FreshIdT Int m) (StdValue m) where
    thunk mv = do
        opts :: Options <- lift $ asks (view hasLens)

        if thunks opts
            then do
                frames :: Frames <- lift $ asks (view hasLens)

                -- Gather the current evaluation context at the time of thunk
                -- creation, and record it along with the thunk.
                let go (fromException ->
                            Just (EvaluatingExpr scope
                                     (Fix (Compose (Ann s e))))) =
                        let e' = Compose (Ann s (Nothing <$ e))
                        in [Provenance scope e']
                    go _ = []
                    ps = concatMap (go . frame) frames

                fmap (StdThunk . NCited ps) . thunk $ mv
            else
                fmap (StdThunk . NCited []) . thunk $ mv

    thunkId = error "jww (2019-03-15): NYI"

    query = error "jww (2019-03-15): NYI"
    queryM = error "jww (2019-03-15): NYI"

    -- The ThunkLoop exception is thrown as an exception with MonadThrow,
    -- which does not capture the current stack frame information to provide
    -- it in a NixException, so we catch and re-throw it here using
    -- 'throwError' from Frames.hs.
    force (StdThunk (NCited ps t)) f =
        catch go (lift . throwError @ThunkLoop)
      where
        go = case ps of
            [] -> force t f
            Provenance scope e@(Compose (Ann s _)):_ -> do
                r <- liftWith $ \run -> do
                    withFrame Info (ForcingExpr scope (wrapExprLoc s e))
                        (run (force t f))
                restoreT $ return r

    forceEff (StdThunk (NCited ps t)) f =
        catch go (lift . throwError @ThunkLoop)
      where
        go = case ps of
            [] -> forceEff t f
            Provenance scope e@(Compose (Ann s _)):_ -> do
                r <- liftWith $ \run -> do
                    withFrame Info (ForcingExpr scope (wrapExprLoc s e))
                        (run (forceEff t f))
                restoreT $ return r

    wrapValue = StdThunk . NCited [] . wrapValue
    getValue (StdThunk (NCited _ v)) = getValue v

instance FromValue NixString m (StdThunk m) where
instance FromValue Path m (StdThunk m) where
instance FromValue [StdThunk m] m (StdThunk m) where
instance FromValue (M.HashMap Text (StdThunk m)) m (StdThunk m) where
instance ToValue NixString m (StdThunk m) where
instance ToValue Int m (StdThunk m) where
instance ToValue () m (StdThunk m) where
instance FromValue [NixString] m (StdThunk m) where
instance FromNix [NixString] m (StdThunk m) where
instance ToValue (StdThunk m) m (NValue (StdThunk m) f m) where
instance ToNix (StdThunk m) m (NValue (StdThunk m) f m) where

runStdLazyM :: MonadIO m => Options -> StdLazy m a -> m a
runStdLazyM opts = runFreshIdT (1 :: Int) . runLazyM opts
