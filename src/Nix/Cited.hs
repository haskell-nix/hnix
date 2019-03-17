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
import           Nix.Value

data Provenance t f m = Provenance
    { _lexicalScope :: Scopes m t
    , _originExpr   :: NExprLocF (Maybe (NValue t f m))
      -- ^ When calling the function x: x + 2 with argument x = 3, the
      --   'originExpr' for the resulting value will be 3 + 2, while the
      --   'contextExpr' will be @(x: x + 2) 3@, preserving not only the
      --   result of the call, but what was called and with what arguments.
    }
    deriving (Generic, Typeable, Show)

data NCited t f m a = NCited
    { _provenance :: [Provenance t f m]
    , _cited      :: a
    }
    deriving (Generic, Typeable, Functor, Foldable, Traversable, Show)

instance Applicative (NCited t f m) where
  pure = NCited []
  NCited xs f <*> NCited ys x = NCited (xs <> ys) (f x)

instance Comonad (NCited t f m) where
  duplicate p = NCited (_provenance p) p
  extract = _cited

instance ComonadEnv [Provenance t f m] (NCited t f m) where
  ask = _provenance

$(makeLenses ''Provenance)
$(makeLenses ''NCited)

class HasCitations t f m a where
    citations :: a -> [Provenance t f m]
    addProvenance :: Provenance t f m -> a -> a

instance HasCitations t f m (NCited t f m a) where
  citations = _provenance
  addProvenance x (NCited p v) = (NCited (x : p) v)

class HasCitations1 t f m where
    citations1 :: f a -> [Provenance t f m]
    addProvenance1 :: Provenance t f m -> f a -> f a
