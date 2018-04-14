{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Nix.Atoms where

import Codec.Serialise
import Control.DeepSeq
import Data.Data
import Data.Text (Text, pack)
import GHC.Generics

-- | Atoms are values that evaluate to themselves. This means that
-- they appear in both the parsed AST (in the form of literals) and
-- the evaluated form.
data NAtom
  -- | An integer. The c nix implementation currently only supports
  -- integers that fit in the range of 'Int64'.
  = NInt Integer
  -- | A floating point number
  | NFloat Float
  -- | Booleans.
  | NBool Bool
  -- | Null values. There's only one of this variant.
  | NNull
  -- | URIs, which are just string literals, but do not need quotes.
  | NUri Text
  deriving (Eq, Ord, Generic, Typeable, Data, Show, NFData, Serialise)

-- | Translate an atom into its nix representation.
atomText :: NAtom -> Text
atomText (NInt i)   = pack (show i)
atomText (NFloat f) = pack (show f)
atomText (NBool b)  = if b then "true" else "false"
atomText NNull      = "null"
atomText (NUri uri) = uri
