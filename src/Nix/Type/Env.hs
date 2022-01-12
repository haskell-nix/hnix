{-# language TypeFamilies #-}

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

import           Nix.Prelude             hiding ( empty
                                                , toList
                                                , fromList
                                                )

import           Nix.Expr.Types
import           Nix.Type.Type

import qualified Data.Map                      as Map


-- * Typing Environment

newtype Env = TypeEnv (Map VarName [Scheme])
  deriving (Eq, Show)

instance Semigroup Env where
  -- | Right-biased merge (override). Analogous to @//@ in @Nix@
  -- Since nature of environment is to update & grow.
  (<>) = mergeRight

instance Monoid Env where
  mempty = empty

instance One Env where
  type OneItem Env = (VarName, Scheme)
  one (x, y) = TypeEnv $ one (x, one y)

empty :: Env
empty = TypeEnv mempty

extend :: Env -> (VarName, [Scheme]) -> Env
extend env (x, s) = coerce (Map.insert x s) env

remove :: Env -> VarName -> Env
remove env var = TypeEnv $ Map.delete var $ coerce env

extends :: Env -> [(VarName, [Scheme])] -> Env
extends env xs = fromList xs <> coerce env

lookup :: VarName -> Env -> Maybe [Scheme]
lookup key tys = Map.lookup key $ coerce tys

merge :: Env -> Env -> Env
merge a b = TypeEnv $ coerce a <> coerce b

mergeRight :: Env -> Env -> Env
mergeRight = flip merge

mergeEnvs :: [Env] -> Env
mergeEnvs = foldl' (<>) mempty

singleton :: VarName -> Scheme -> Env
singleton = curry one

keys :: Env -> [VarName]
keys (TypeEnv env) = Map.keys env

fromList :: [(VarName, [Scheme])] -> Env
fromList xs = coerce $ Map.fromList xs

toList :: Env -> [(VarName, [Scheme])]
toList (TypeEnv env) = Map.toList env

