{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms    #-}

module Nix.Cited.Basic where

import           Prelude                 hiding ( force )
import           Control.Comonad                ( Comonad )
import           Control.Comonad.Env            ( ComonadEnv )
import           Control.Monad.Catch     hiding ( catchJust )
import           Nix.Cited
import           Nix.Eval                      as Eval
                                                ( EvalFrame(EvaluatingExpr,ForcingExpr) )
import           Nix.Exec
import           Nix.Expr.Types.Annotated
import           Nix.Frames
import           Nix.Options
import           Nix.Thunk
import           Nix.Utils
import           Nix.Value


-- * data type @Cited@

newtype Cited t f m a = Cited (NCited m (NValue t f m) a)
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


-- ** Helpers

-- | @Cited@ pattern.
-- > pattern CitedP m a = Cited (NCited m a)
pattern CitedP
  :: [Provenance m (NValue t f m)]
  -> a
  -> Cited t f m a
pattern CitedP m a = Cited (NCited m a)
{-# complete CitedP #-}

-- | Take:
-- 1. Provenence info.
-- 2. Value (like thunk)
-- -> Produce cited value (thunk)
cite
  :: Functor m
  => [Provenance m (NValue t f m)]
  -> m a
  -> m (Cited t f m a)
cite v = fmap (Cited . NCited v)


-- ** instances

instance
  HasCitations1 m (NValue t f m) (Cited t f m)
 where

  citations1 (Cited c) = citations c
  addProvenance1 x (Cited c) = Cited $ addProvenance x c

instance
  ( Has e Options
  , Framed e m
  , MonadThunk t m v
  , Typeable m
  , Typeable f
  , Typeable u
  , MonadCatch m
  )
  => MonadThunk (Cited u f m t) m v where

  thunk :: m v -> m (Cited u f m t)
  thunk mv =
    do
      opts :: Options <- asks $ view hasLens

      bool
        (cite mempty)
        (\ t ->
          do
            frames :: Frames <- asks $ view hasLens

            -- Gather the current evaluation context at the time of thunk
            -- creation, and record it along with the thunk.
            let
              go (fromException -> Just (EvaluatingExpr scope (AnnE s e))) =
                let e' = AnnFP s (Nothing <$ e) in
                [Provenance scope e']
              go _ = mempty
              ps = concatMap (go . frame) frames

            cite ps t
        )
        (thunks opts)
        (thunk mv)

  thunkId :: Cited u f m t -> ThunkId m
  thunkId (CitedP _ t) = thunkId @_ @m t

  query :: m v -> Cited u f m t -> m v
  query m (CitedP _ t) = query m t

  -- | The ThunkLoop exception is thrown as an exception with MonadThrow,
  --   which does not capture the current stack frame information to provide
  --   it in a NixException, so we catch and re-throw it here using
  --   'throwError' from Frames.hs.
  force :: Cited u f m t -> m v
  force (CitedP ps t) = handleDisplayProvenance ps $ force t

  forceEff :: Cited u f m t -> m v
  forceEff (CitedP ps t) = handleDisplayProvenance ps $ forceEff t

  further :: Cited u f m t -> m (Cited u f m t)
  further (CitedP ps t) = cite ps $ further t


-- ** Kleisli functor HOFs

-- Please, do not use MonadThunkF for MonadThunk, later uses more straight-forward specialized line of functions.
instance
  ( Has e Options
  , Framed e m
  , MonadThunkF t m v
  , Typeable m
  , Typeable f
  , Typeable u
  , MonadCatch m
  )
  => MonadThunkF (Cited u f m t) m v where

  queryF :: (v -> m r) -> m r -> Cited u f m t -> m r
  queryF k m (CitedP _ t) = queryF k m t

  forceF :: (v -> m r) -> Cited u f m t -> m r
  forceF k (CitedP ps t) = handleDisplayProvenance ps $ forceF k t

  forceEffF :: (v -> m r) -> Cited u f m t -> m r
  forceEffF k (CitedP ps t) = handleDisplayProvenance ps $ forceEffF k t

  furtherF :: (m v -> m v) -> Cited u f m t -> m (Cited u f m t)
  furtherF k (CitedP ps t) = cite ps $ furtherF k t


-- * Representation

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
    (\ (Provenance scope e@(AnnFP s _) : _) ->
      withFrame Info $ ForcingExpr scope $ wrapExprLoc s e
    )
