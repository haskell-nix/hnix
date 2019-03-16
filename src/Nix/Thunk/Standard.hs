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
import           Control.Monad.Ref
import           Data.Fix
import           Data.GADT.Compare
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

newtype NThunk f m = NThunk
    { _nThunk :: NCited (NThunk f m) (NValue (NThunk f m) f m) m
                       (NThunkF m (NValue (NThunk f m) f m)) }

instance (MonadNix e t f m, MonadFreshId Int m, MonadAtomicRef m, GEq (Ref m))
  => MonadThunk (NThunk f m) m (NValue (NThunk f m) f m) where
    thunk mv = do
        opts :: Options <- asks (view hasLens)

        if thunks opts
            then do
                frames :: Frames <- asks (view hasLens)

                -- Gather the current evaluation context at the time of thunk
                -- creation, and record it along with the thunk.
                let go (fromException ->
                            Just (EvaluatingExpr scope
                                     (Fix (Compose (Ann span e))))) =
                        let e' = Compose (Ann span (Nothing <$ e))
                        in [Provenance scope e']
                    go _ = []
                    ps = concatMap (go . frame) frames

                fmap (NThunk . NCited ps) . thunk $ mv
            else
                fmap (NThunk . NCited []) . thunk $ mv

    -- The ThunkLoop exception is thrown as an exception with MonadThrow,
    -- which does not capture the current stack frame information to provide
    -- it in a NixException, so we catch and re-throw it here using
    -- 'throwError' from Frames.hs.
    force (NThunk (NCited ps t)) f = catch go (throwError @ThunkLoop)
      where
        go = case ps of
            [] -> force t f
            Provenance scope e@(Compose (Ann span _)):_ ->
                withFrame Info (ForcingExpr scope (wrapExprLoc span e))
                    (force t f)

    forceEff (NThunk (NCited ps t)) f = catch go (throwError @ThunkLoop)
      where
        go = case ps of
            [] -> forceEff t f
            Provenance scope e@(Compose (Ann span _)):_ ->
                withFrame Info (ForcingExpr scope (wrapExprLoc span e))
                    (forceEff t f)

    wrapValue = NThunk . NCited [] . wrapValue
    getValue (NThunk (NCited _ v)) = getValue v

-- instance Monad m => MonadFreshId Int (Lazy t f m) where
--   freshId = Lazy $ lift $ lift freshId

instance FromValue NixString m (NThunk f m) where
instance FromValue Path m (NThunk f m) where
instance FromValue [NThunk f m] m (NThunk f m) where
instance FromValue (M.HashMap Text (NThunk f m)) m (NThunk f m) where
instance ToValue NixString m (NThunk f m) where
instance ToValue Int m (NThunk f m) where
instance ToValue () m (NThunk f m) where
instance FromValue [NixString] m (NThunk f m) where
instance FromNix [NixString] m (NThunk f m) where
instance ToValue (NThunk f m) m (NValue (NThunk f m) f m) where
instance ToNix (NThunk f m) m (NValue (NThunk f m) f m) where

