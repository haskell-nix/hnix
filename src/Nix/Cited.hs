{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Nix.Cited where

import Control.Comonad
import Control.Comonad.Env
import Lens.Family2.TH

import Data.Typeable (Typeable)
import GHC.Generics

import Nix.Expr.Types.Annotated
import Nix.Scope

data Provenance t v m = Provenance
    { _lexicalScope :: Scopes m t
    , _originExpr   :: NExprLocF (Maybe v)
      -- ^ When calling the function x: x + 2 with argument x = 3, the
      --   'originExpr' for the resulting value will be 3 + 2, while the
      --   'contextExpr' will be @(x: x + 2) 3@, preserving not only the
      --   result of the call, but what was called and with what arguments.
    }
    deriving (Generic, Typeable)

data NCited t v m a = NCited
    { _provenance :: [Provenance t v m]
    , _cited      :: a
    }
    deriving (Generic, Typeable, Functor, Foldable, Traversable)

instance Applicative (NCited t v m) where
  pure = NCited []
  -- jww (2019-03-11): ??
  NCited xs f <*> NCited ys x = NCited (xs <> ys) (f x)

instance Comonad (NCited t v m) where
  duplicate p = NCited (_provenance p) p
  extract = _cited

instance ComonadEnv [Provenance t v m] (NCited t v m) where
  ask = _provenance

$(makeLenses ''Provenance)
$(makeLenses ''NCited)
