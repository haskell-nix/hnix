{-# language TypeFamilies #-}

-- | Basing on the Nix (Hindley–Milner) type system (that provides decidable type inference):
-- gathering assumptions (inference evidence) about polymorphic types.
module Nix.Type.Assumption
  ( Assumption(..)
  , empty
  , lookup
  , remove
  , extend
  , keys
  , merge
  , singleton
  )
where

import           Nix.Prelude             hiding ( Type
                                                , empty
                                                )

import qualified Data.HashMap.Strict           as HM

import           Nix.Expr.Types
import           Nix.Type.Type

-- | Assumptions map variable names to lists of types.
-- Keys can have multiple types (the consistency between assumptions is the inference responsibility).
-- Uses HashMap for O(1) average lookup and remove operations.
newtype Assumption = Assumption (HM.HashMap VarName [Type])
  deriving (Eq, Show)

instance Semigroup Assumption where
  (<>) = merge

instance Monoid Assumption where
  mempty = empty

instance One Assumption where
  type OneItem Assumption = (VarName, Type)
  one (var, ty) = Assumption $ HM.singleton var [ty]

empty :: Assumption
empty = Assumption HM.empty

-- | Add a type for a variable (prepends to existing types for that variable).
extend :: Assumption -> (VarName, Type) -> Assumption
extend (Assumption m) (var, ty) =
  Assumption $ HM.insertWith (++) var [ty] m

-- | Remove all types for a variable.
remove :: Assumption -> VarName -> Assumption
remove (Assumption m) var =
  Assumption $ HM.delete var m

-- | Look up all types for a variable.
lookup :: VarName -> Assumption -> [Type]
lookup key (Assumption m) =
  fromMaybe [] $ HM.lookup key m

-- | Merge two assumptions by combining type lists for each variable.
merge :: Assumption -> Assumption -> Assumption
merge (Assumption m1) (Assumption m2) =
  Assumption $ HM.unionWith (++) m1 m2

singleton :: VarName -> Type -> Assumption
singleton = curry one

keys :: Assumption -> [VarName]
keys (Assumption m) = HM.keys m
