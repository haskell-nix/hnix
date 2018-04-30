module Nix.Type.Type where

import Data.Text (Text)

newtype TVar = TV String
  deriving (Show, Eq, Ord)

data Type
  = TVar TVar                   -- type variable
  | TCon String                 -- known type
  | TArr Type Type              -- type -> type
  deriving (Show, Eq, Ord)

data Scheme = Forall [TVar] Type -- forall a b. a -> b
  deriving (Show, Eq, Ord)

typeInt, typeFloat, typeBool, typePath, typeUri, typeNull :: Type
typeInt    = TCon "Int"
typeFloat  = TCon "Float"
typeBool   = TCon "Bool"
typeString = TCon "String"
typePath   = TCon "Path"
typeUri    = TCon "URI"
typeNull   = TCon "Null"

type Name = Text
