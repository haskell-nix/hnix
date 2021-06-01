{-# LANGUAGE TypeFamilies #-}
module Nix.Type.Env
  ( Env(..)
  , empty
  , lookup
  , remove
  , extend
  , extends
  , merge
  , mergeEnvs
  , singleton
  , keys
  , fromList
  , toList
  )
where

import           Prelude                 hiding ( empty
                                                , toList
                                                , fromList
                                                )

import           Nix.Type.Type

import qualified Data.Map                      as Map


-- * Typing Environment

newtype Env = TypeEnv (Map.Map Name [Scheme])
  deriving (Eq, Show)

instance Semigroup Env where
  -- | Right-biased merge (override). Analogous to @//@ in @Nix@
  -- Since nature of environment is to update & grow.
  (<>) = mergeRight

instance Monoid Env where
  mempty = empty

instance One Env where
  type OneItem Env = (Name, Scheme)
  one = uncurry singleton

empty :: Env
empty = TypeEnv mempty

extend :: Env -> (Name, [Scheme]) -> Env
extend env (x, s) = TypeEnv $ Map.insert x s $ coerce env

remove :: Env -> Name -> Env
remove (TypeEnv env) var = TypeEnv $ Map.delete var env

extends :: Env -> [(Name, [Scheme])] -> Env
extends env xs = TypeEnv $ Map.fromList xs `Map.union` coerce env

lookup :: Name -> Env -> Maybe [Scheme]
lookup key (TypeEnv tys) = Map.lookup key tys

merge :: Env -> Env -> Env
merge (TypeEnv a) (TypeEnv b) = TypeEnv $ a `Map.union` b

mergeRight :: Env -> Env -> Env
mergeRight (TypeEnv a) (TypeEnv b) = TypeEnv $ b `Map.union` a

mergeEnvs :: [Env] -> Env
mergeEnvs = foldl' (<>) mempty

singleton :: Name -> Scheme -> Env
singleton x y = TypeEnv $ one (x, [y])

keys :: Env -> [Name]
keys (TypeEnv env) = Map.keys env

fromList :: [(Name, [Scheme])] -> Env
fromList xs = TypeEnv $ Map.fromList xs

toList :: Env -> [(Name, [Scheme])]
toList (TypeEnv env) = Map.toList env

