{-# language DeriveAnyClass #-}
{-# language TemplateHaskell #-}

{-# options_ghc -Wno-missing-signatures #-}

module Nix.Cited where

import           Control.Comonad
import           Control.Comonad.Env
import           Lens.Family2.TH

import           Nix.Expr.Types.Annotated
import           Nix.Scope
import           Nix.Value                      ( NValue, NValue'(NValue') )
import           Control.Monad.Free             ( Free(Pure, Free) )

data Provenance m v =
  Provenance
    { getLexicalScope :: Scopes m v
      --  2021-11-09: NOTE: Better name?
    , getOriginExpr   :: NExprLocF (Maybe v)
      -- ^ When calling the function x: x + 2 with argument x = 3, the
      --   'originExpr' for the resulting value will be 3 + 2, while the
      --   'contextExpr' will be @(x: x + 2) 3@, preserving not only the
      --   result of the call, but what was called and with what arguments.
    }
    deriving (Generic, Typeable, Show)

data NCited m v a =
  NCited
    { getProvenance :: [Provenance m v]
    , getCited      :: a
    }
    deriving (Generic, Typeable, Functor, Foldable, Traversable, Show)

instance Applicative (NCited m v) where
  pure = NCited mempty
  (<*>) (NCited xs f) (NCited ys x) = NCited (xs <> ys) (f x)

instance Comonad (NCited m v) where
  duplicate p = NCited (getProvenance p) p
  extract = getCited

instance ComonadEnv [Provenance m v] (NCited m v) where
  ask = getProvenance

$(makeLenses ''Provenance)
$(makeLenses ''NCited)

class HasCitations1 m v f where
  citations1 :: f a -> [Provenance m v]
  addProvenance1 :: Provenance m v -> f a -> f a

class HasCitations m v a where
  citations :: a -> [Provenance m v]
  addProvenance :: Provenance m v -> a -> a

instance HasCitations m v (NCited m v a) where
  citations = getProvenance
  addProvenance x (NCited p v) = NCited (x : p) v

instance HasCitations1 m v f
  => HasCitations m v (NValue' t f m a) where
  citations (NValue' f) = citations1 f
  addProvenance x (NValue' f) = NValue' $ addProvenance1 x f

instance (HasCitations1 m v f, HasCitations m v t)
  => HasCitations m v (NValue t f m) where
  citations (Pure t) = citations t
  citations (Free v) = citations v
  addProvenance x (Pure t) = Pure $ addProvenance x t
  addProvenance x (Free v) = Free $ addProvenance x v
