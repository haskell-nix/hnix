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

import           Nix.Expr.Types
import           Nix.Type.Type

newtype Assumption = Assumption [(VarName, Type)]
  deriving (Eq, Show)

-- We pretend that Assumptions can be inconsistent (nonunique keys),
-- (just like people in real life).
-- The consistency between assumptions is the inference responcibility.
instance Semigroup Assumption where
  (<>) = merge

instance Monoid Assumption where
  mempty = empty

instance One Assumption where
  type OneItem Assumption = (VarName, Type)
  one vt = Assumption $ one vt

--  2022-01-12: NOTE: `empty` implies Alternative. Either have Alternative or use `mempty`
empty :: Assumption
empty = Assumption mempty

extend :: Assumption -> (VarName, Type) -> Assumption
extend a vt =
  one (coerce vt) <> a

remove :: Assumption -> VarName -> Assumption
remove a var =
  coerce
    rmVar
    a
 where
  rmVar :: [(VarName, Type)] -> [(VarName, Type)]
  rmVar =
    filter
      ((/=) var . fst)

lookup :: VarName -> Assumption -> [Type]
lookup key a =
  snd <$>
    filter
      ((==) key . fst)
      (coerce a)

merge :: Assumption -> Assumption -> Assumption
merge =
  coerce ((<>) @[(VarName, Type)])

singleton :: VarName -> Type -> Assumption
singleton = curry one

keys :: Assumption -> [VarName]
keys (Assumption a) = fst <$> a
