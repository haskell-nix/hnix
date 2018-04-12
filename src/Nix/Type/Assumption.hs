module Nix.Type.Assumption (
  Assumption(..),
  empty,
  lookup,
  remove,
  extend,
  keys,
  merge,
  mergeAssumptions,
  singleton,
) where

import Prelude hiding (lookup)

import Nix.Type.Type

import Data.Foldable

newtype Assumption = Assumption { assumptions :: [(Name, Type)] }
  deriving (Eq, Show)

empty :: Assumption
empty = Assumption []

extend :: Assumption -> (Name, Type) -> Assumption
extend (Assumption a) (x, s) = Assumption ((x, s) : a)

remove :: Assumption -> Name -> Assumption
remove (Assumption a) var = Assumption (filter (\(n, _) -> n /= var) a)

lookup :: Name -> Assumption -> [Type]
lookup key (Assumption a) = map snd (filter (\(n, _) -> n == key) a)

merge :: Assumption -> Assumption -> Assumption
merge (Assumption a) (Assumption b) = Assumption (a ++ b)

mergeAssumptions :: [Assumption] -> Assumption
mergeAssumptions = foldl' merge empty

singleton :: Name -> Type -> Assumption
singleton x y = Assumption [(x, y)]

keys :: Assumption -> [Name]
keys (Assumption a) = map fst a
