module Nix.Type.Type where

import           Prelude                 hiding ( Type, TVar )
import           Data.Foldable                  ( foldr1 )
import           Nix.Utils                      ( AttrSet )

type Name = Text

-- | Hindrey-Milner type interface

newtype TVar = TV Text
  deriving (Show, Eq, Ord)

data Type
  = TVar TVar                -- type variable
  | TCon Text                -- known type
  | TSet Bool (AttrSet Type) -- heterogeneous map, bool if variadic
  | TList [Type]             -- heterogeneous list
  | (:~>) Type Type          -- type -> type
  | TMany [Type]             -- variant type
  deriving (Show, Eq, Ord)

data Scheme = Forall [TVar] Type -- forall a b. a -> b
  deriving (Show, Eq, Ord)

-- This models a set that unifies with any other set.
typeSet :: Type
typeSet = TSet True mempty

typeList :: Type
typeList = TList mempty

infixr 1 :~>

typeFun :: [Type] -> Type
-- Please, replace with safe analog to `foldr1`
typeFun = foldr1 (:~>)

typeInt, typeFloat, typeBool, typeString, typePath, typeNull :: Type
typeInt    = TCon "integer"
typeFloat  = TCon "float"
typeBool   = TCon "boolean"
typeString = TCon "string"
typePath   = TCon "path"
typeNull   = TCon "null"
