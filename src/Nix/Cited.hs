{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
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

data Provenance m v = Provenance
    { _lexicalScope :: Scopes m v
    , _originExpr   :: NExprLocF (Maybe v)
      -- ^ When calling the function x: x + 2 with argument x = 3, the
      --   'originExpr' for the resulting value will be 3 + 2, while the
      --   'contextExpr' will be @(x: x + 2) 3@, preserving not only the
      --   result of the call, but what was called and with what arguments.
    }
    deriving (Generic, Typeable, Show)

data NCited m v a = NCited
    { _provenance :: [Provenance m v]
    , _cited      :: a
    }
    deriving (Generic, Typeable, Functor, Foldable, Traversable, Show)

instance Applicative (NCited m v) where
  pure = NCited []
  NCited xs f <*> NCited ys x = NCited (xs <> ys) (f x)

instance Comonad (NCited m v) where
  duplicate p = NCited (_provenance p) p
  extract = _cited

instance ComonadEnv [Provenance m v] (NCited m v) where
  ask = _provenance

$(makeLenses ''Provenance)
$(makeLenses ''NCited)

class HasCitations m v a where
    citations :: a -> [Provenance m v]
    addProvenance :: Provenance m v -> a -> a

instance HasCitations m v (NCited m v a) where
  citations = _provenance
  addProvenance x (NCited p v) = (NCited (x : p) v)

class HasCitations1 m v f where
    citations1 :: f a -> [Provenance m v]
    addProvenance1 :: Provenance m v -> f a -> f a
