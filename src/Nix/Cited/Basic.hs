{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE InstanceSigs #-}

module Nix.Cited.Basic where

import           Prelude                 hiding ( force )
import           Control.Comonad                ( Comonad )
import           Control.Comonad.Env            ( ComonadEnv )
import           Control.Monad.Catch     hiding ( catchJust )
import           Nix.Cited
import           Nix.Eval                      as Eval
import           Nix.Exec
import           Nix.Expr
import           Nix.Frames
import           Nix.Options
import           Nix.Thunk
import           Nix.Utils
import           Nix.Value

newtype Cited t f m a = Cited { getCited :: NCited m (NValue t f m) a }
  deriving
    ( Generic
    , Typeable
    , Functor
    , Applicative
    , Foldable
    , Traversable
    , Comonad
    , ComonadEnv [Provenance m (NValue t f m)]
    )

instance HasCitations1 m (NValue t f m) (Cited t f m) where
  citations1 (Cited c) = citations c
  addProvenance1 x (Cited c) = Cited $ addProvenance x c

instance ( Has e Options
         , Framed e m
         , MonadThunk t m v
         , Typeable m
         , Typeable f
         , Typeable u
         , MonadCatch m
         )
  => MonadThunk (Cited u f m t) m v where

  thunk :: m v -> m (Cited u f m t)
  thunk mv = do
    opts :: Options <- asks (view hasLens)

    bool
      (Cited . NCited mempty <$> thunk mv)
      (do
        frames :: Frames <- asks (view hasLens)

        -- Gather the current evaluation context at the time of thunk
        -- creation, and record it along with the thunk.
        let
          go (fromException -> Just (EvaluatingExpr scope (AnnE s e))) =
            let e' = Compose (Ann s (Nothing <$ e)) in
            [Provenance scope e']
          go _ = mempty
          ps = concatMap (go . frame) frames

        Cited . NCited ps <$> thunk mv
      )
      (thunks opts)

  thunkId :: Cited u f m t -> ThunkId m
  thunkId (Cited (NCited _ t)) = thunkId @_ @m t

  queryM :: m v -> Cited u f m t -> m v
  queryM m (Cited (NCited _ t)) = queryM m t

  -- | The ThunkLoop exception is thrown as an exception with MonadThrow,
  --   which does not capture the current stack frame information to provide
  --   it in a NixException, so we catch and re-throw it here using
  --   'throwError' from Frames.hs.
  force :: Cited u f m t -> m v
  force (Cited (NCited ps t)) = handleDisplayProvenance ps $ force t

  forceEff :: Cited u f m t -> m v
  forceEff (Cited (NCited ps t)) = handleDisplayProvenance ps $ forceEff t

  further :: Cited u f m t -> m (Cited u f m t)
  further (Cited (NCited ps t)) = Cited . NCited ps <$> further t


-- * Kleisli functor HOFs

-- Please, do not use MonadThunkF for MonadThunk, later uses more straight-forward specialized line of functions.
instance ( Has e Options
         , Framed e m
         , MonadThunkF t m v
         , Typeable m
         , Typeable f
         , Typeable u
         , MonadCatch m
         )
  => MonadThunkF (Cited u f m t) m v where

  queryMF :: (v -> m r) -> m r -> Cited u f m t -> m r
  queryMF k m (Cited (NCited _ t)) = queryMF k m t

  forceF :: (v -> m r) -> Cited u f m t -> m r
  forceF k (Cited (NCited ps t)) = handleDisplayProvenance ps $ forceF k t

  forceEffF :: (v -> m r) -> Cited u f m t -> m r
  forceEffF k (Cited (NCited ps t)) = handleDisplayProvenance ps $ forceEffF k t

  furtherF :: (m v -> m v) -> Cited u f m t -> m (Cited u f m t)
  furtherF k (Cited (NCited ps t)) = Cited . NCited ps <$> furtherF k t


-- ** Utils

handleDisplayProvenance
  :: (MonadCatch m
    , Typeable m
    , Typeable v
    , Has e Frames
    , MonadReader e m
    )
  => [Provenance m v]
  -> m a
  -> m a
handleDisplayProvenance ps f =
  catch
    (displayProvenance ps f)
    (throwError @ThunkLoop)

displayProvenance
  :: (MonadThrow m
    , MonadReader e m
    , Has e Frames
    , Typeable m
    , Typeable v
    )
  => [Provenance m v]
  -> m a
  -> m a
displayProvenance =
  list
    id
    (\ (Provenance scope e@(Compose (Ann s _)) : _) ->
      withFrame Info (ForcingExpr scope (wrapExprLoc s e))
    )
