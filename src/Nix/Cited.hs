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

import Control.Comonad
import Control.Comonad.Env
import Data.Functor.Compose
import Data.Typeable (Typeable)
import GHC.Generics
import Lens.Family2.TH
import Text.Show.Deriving

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
    deriving (Generic, Typeable, Show)

data NCited t v m a = NCited
    { _provenance :: [Provenance t v m]
    , _cited      :: a
    }
    deriving (Generic, Typeable, Functor, Foldable, Traversable, Show)

$(deriveShow1 ''NCited)

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

class HasCitations t v m a where
    citations :: a -> [Provenance t v m]
    addProvenance :: Provenance t v m -> a -> a

-- addProvenance :: (NValue t f m a -> Provenance t (NValue t f m a) m) -> NValue t f m a -> NValue t f m a
-- addProvenance f l@(NValue (NCited p v)) = NValue (NCited (f l : p) v)

instance HasCitations t v m (NCited t v m a) where
    citations = _provenance
    addProvenance x (NCited p v) = (NCited (x : p) v)

class HasCitations1 t v m f where
    citations1 :: f a -> [Provenance t v m]
    addProvenance1 :: Provenance t v m -> f a -> f a

instance HasCitations1 t v m f => HasCitations1 t v m (Compose f g) where
    citations1 (Compose f) = citations1 f
    addProvenance1 x (Compose f) = Compose (addProvenance1 x f)
