-- | The basis of the Nix type system (type-level).
--   Based on the Hindley–Milner type system.
--   Therefore -> from this the type inference follows.
module Nix.Type.Type where

import           Nix.Prelude                 hiding ( Type, TVar )
import           Nix.Expr.Types

-- | Hindrey-Milner type interface

-- | Type variable in the Nix type system.
newtype TVar = TV Text
  deriving (Show, Eq, Ord)

-- | The basic type definitions in the Nix type system (type-level code).
data Type
  = TVar TVar                -- ^ Type variable in the Nix type system.
  | TCon Text                -- ^ Concrete (non-polymorphic, constant) type in the Nix type system.
  | TSet Variadic (AttrSet Type) -- ^ Heterogeneous map in the Nix type system. @True@ -> variadic.
  | TList [Type]             -- ^ Heterogeneous list in the Nix type system.
  | (:~>) Type Type          -- ^ Type arrow (@Type -> Type@) in the Nix type system.
  | TMany [Type]             -- ^ Variant type (term). Since relating to Nix type system, more precicely -
                             --   dynamic types in dynamicly typed language (which is Nix).
  deriving (Show, Eq, Ord)

infixr 1 :~>

-- | Hindley–Milner type system uses "scheme" term for "polytypes".
--   Types containing @forall@ quantifiers: @forall a . a@.
--   Note: HM allows only top-level @forall@ quantification, so no @RankNTypes@ in it.
data Scheme = Forall [TVar] Type -- ^ @Forall [TVar] Type@: the Nix type system @forall vars. type@.
  deriving (Show, Eq, Ord)

-- | Concrete types in the Nix type system.
typeNull, typeBool, typeInt, typeFloat, typeString, typePath :: Type
typeNull   = TCon "null"
typeBool   = TCon "boolean"
typeInt    = TCon "integer"
typeFloat  = TCon "float"
typeString = TCon "string"
typePath   = TCon "path"

-- This models a set that unifies with any other set.
typeSet :: Type
typeSet = TSet mempty mempty

typeList :: Type
typeList = TList mempty

typeFun :: NonEmpty Type -> Type
typeFun (head_ :| tail_) = foldr (:~>) head_ tail_
