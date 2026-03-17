{- | The basis of the Nix type system (type-level).
  Based on the Hindley–Milner type system.
  Therefore -> from this the type inference follows.
-}
module Nix.Type.Type where

import Nix.Expr.Types
import Nix.Prelude hiding (TVar, Type)

-- | Hindrey-Milner type interface

-- | Type variable in the Nix type system.
newtype TVar = TV Text
    deriving (Show, Eq, Ord)

-- | The basic type definitions in the Nix type system (type-level code).
data Type
    = -- | Type variable in the Nix type system.
      TVar TVar
    | -- | Concrete (non-polymorphic, constant) type in the Nix type system.
      TCon Text
    | -- | Heterogeneous map in the Nix type system. @True@ -> variadic.
      TSet Variadic (AttrSet Type)
    | -- | Heterogeneous list in the Nix type system.
      TList [Type]
    | -- | Type arrow (@Type -> Type@) in the Nix type system.
      (:~>) Type Type
    | -- | Variant type (term). Since relating to Nix type system, more precicely -
      --       dynamic types in dynamicly typed language (which is Nix).
      TMany [Type]
    deriving (Show, Eq, Ord)

infixr 1 :~>

{- | Hindley–Milner type system uses "scheme" term for "polytypes".
  Types containing @forall@ quantifiers: @forall a . a@.
  Note: HM allows only top-level @forall@ quantification, so no @RankNTypes@ in it.
-}
data Scheme
    = -- | @Forall [TVar] Type@: the Nix type system @forall vars. type@.
      Forall [TVar] Type
    deriving (Show, Eq, Ord)

-- | Concrete types in the Nix type system.
typeNull, typeBool, typeInt, typeFloat, typeString, typePath :: Type
typeNull = TCon "null"
typeBool = TCon "boolean"
typeInt = TCon "integer"
typeFloat = TCon "float"
typeString = TCon "string"
typePath = TCon "path"

-- This models a set that unifies with any other set.
typeSet :: Type
typeSet = TSet mempty mempty

typeList :: Type
typeList = TList mempty

typeFun :: NonEmpty Type -> Type
typeFun (head_ :| tail_) = foldr (:~>) head_ tail_
