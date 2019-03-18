{-# LANGUAGE DeriveAnyClass #-}
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

import           Control.Comonad
import           Control.Comonad.Env
import           Data.Typeable                  ( Typeable )
import           GHC.Generics
import           Lens.Family2.TH

import           Nix.Expr.Types.Annotated
import           Nix.Scope

data Provenance t m v = Provenance
    { _lexicalScope :: Scopes m t
    , _originExpr   :: NExprLocF (Maybe v)
      -- ^ When calling the function x: x + 2 with argument x = 3, the
      --   'originExpr' for the resulting value will be 3 + 2, while the
      --   'contextExpr' will be @(x: x + 2) 3@, preserving not only the
      --   result of the call, but what was called and with what arguments.
    }
    deriving (Generic, Typeable, Show)

data NCited t m v a = NCited
    { _provenance :: [Provenance t m v]
    , _cited      :: a
    }
    deriving (Generic, Typeable, Functor, Foldable, Traversable, Show)

instance Applicative (NCited t m v) where
  pure = NCited []
  NCited xs f <*> NCited ys x = NCited (xs <> ys) (f x)

instance Comonad (NCited t m v) where
  duplicate p = NCited (_provenance p) p
  extract = _cited

instance ComonadEnv [Provenance t m v] (NCited t m v) where
  ask = _provenance

$(makeLenses ''Provenance)
$(makeLenses ''NCited)

class HasCitations t m v a where
    citations :: a -> [Provenance t m v]
    addProvenance :: Provenance t m v -> a -> a

instance HasCitations t m v (NCited t m v a) where
  citations = _provenance
  addProvenance x (NCited p v) = (NCited (x : p) v)

class HasCitations1 t m v f where
    citations1 :: f a -> [Provenance t m v]
    addProvenance1 :: Provenance t m v -> f a -> f a
