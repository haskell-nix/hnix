module Nix.Type.Type where

import qualified Data.HashMap.Lazy as M
import           Data.Text (Text)
import           Nix.Utils

newtype TVar = TV String
  deriving (Show, Eq, Ord)

data Type
  = TVar TVar                -- type variable
  | TCon String              -- known type
  | TSet Bool (AttrSet Type) -- heterogenous map, bool if variadic
  | TList [Type]             -- heterogenous list
  | TArr Type Type           -- type -> type
  deriving (Show, Eq, Ord)

data Scheme = Forall [TVar] Type -- forall a b. a -> b
  deriving (Show, Eq, Ord)

-- This models a set that unifies with any other set.
typeSet :: Type
typeSet = TSet True M.empty

typeList :: Type
typeList = TList []

typeFun :: [Type] -> Type
typeFun = foldr1 TArr

typeInt, typeFloat, typeBool, typeString, typePath, typeUri, typeNull :: Type
typeInt    = TCon "integer"
typeFloat  = TCon "float"
typeBool   = TCon "boolean"
typeString = TCon "string"
typePath   = TCon "path"
typeUri    = TCon "uri"
typeNull   = TCon "null"

type Name = Text
