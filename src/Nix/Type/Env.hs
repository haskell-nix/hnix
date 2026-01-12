{-# language TypeFamilies #-}

module Nix.Type.Env
  ( Env(..)
  , empty
  , lookup
  , remove
  , extend
  , merge
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

import qualified Data.HashMap.Strict           as HM


-- * Typing Environment

newtype Env = TypeEnv (HM.HashMap VarName [Scheme])
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
extend env (x, s) = coerce (HM.insert x s) env

remove :: Env -> VarName -> Env
remove env var = TypeEnv $ HM.delete var $ coerce env

lookup :: VarName -> Env -> Maybe [Scheme]
lookup key tys = HM.lookup key $ coerce tys

merge :: Env -> Env -> Env
merge a b = TypeEnv $ coerce a <> coerce b

mergeRight :: Env -> Env -> Env
mergeRight = flip merge

singleton :: VarName -> Scheme -> Env
singleton = curry one

keys :: Env -> [VarName]
keys (TypeEnv env) = HM.keys env

fromList :: [(VarName, [Scheme])] -> Env
fromList xs = coerce $ HM.fromList xs

toList :: Env -> [(VarName, [Scheme])]
toList (TypeEnv env) = HM.toList env
