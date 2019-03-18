{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
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

module Nix.Cited.Basic where

import           Control.Comonad                ( Comonad )
import           Control.Comonad.Env            ( ComonadEnv )
import           Control.Monad.Catch     hiding ( catchJust )
import           Control.Monad.Reader
import           Data.Fix
import           GHC.Generics
import           Nix.Cited
import           Nix.Eval                      as Eval
import           Nix.Exec
import           Nix.Expr
import           Nix.Frames
import           Nix.Options
import           Nix.Thunk
import           Nix.Utils
import           Nix.Value

newtype Cited t f m a = Cited { getCited :: NCited t m (NValue t f m) a }
  deriving
    ( Generic
    , Typeable
    , Functor
    , Applicative
    , Foldable
    , Traversable
    , Comonad
    , ComonadEnv [Provenance t m (NValue t f m)]
    )

instance HasCitations1 t m (NValue t f m) (Cited t f m) where
  citations1 (Cited c) = citations c
  addProvenance1 x (Cited c) = Cited (addProvenance x c)

instance ( Has e Options
         , Framed e m
         , MonadThunk t m v
         , Typeable m
         , Typeable u
         , MonadCatch m
         )
  => MonadThunk (Cited u f m t) m v where
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

        fmap (Cited . NCited ps) . thunk $ mv
      else fmap (Cited . NCited []) . thunk $ mv

  thunkId (Cited (NCited _ t)) = thunkId @_ @m t

  query (Cited (NCited _ t)) = query t
  queryM (Cited (NCited _ t)) = queryM t

  -- | The ThunkLoop exception is thrown as an exception with MonadThrow,
  --   which does not capture the current stack frame information to provide
  --   it in a NixException, so we catch and re-throw it here using
  --   'throwError' from Frames.hs.
  force (Cited (NCited ps t)) f =
    catch go (throwError @ThunkLoop)
   where
    go = case ps of
      [] -> force t f
      Provenance scope e@(Compose (Ann s _)) : _ ->
        withFrame Info (ForcingExpr scope (wrapExprLoc s e)) (force t f)

  forceEff (Cited (NCited ps t)) f = catch
    go
    (throwError @ThunkLoop)
   where
    go = case ps of
      [] -> forceEff t f
      Provenance scope e@(Compose (Ann s _)) : _ ->
        withFrame Info (ForcingExpr scope (wrapExprLoc s e)) (forceEff t f)

  wrapValue = Cited . NCited [] . wrapValue
  getValue (Cited (NCited _ v)) = getValue v
