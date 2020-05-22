{-# LANGUAGE CPP            #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Nix.Atoms where

#ifdef MIN_VERSION_serialise
import Codec.Serialise
#endif
import           Control.DeepSeq
import           Data.Data
import           Data.Fixed                     (mod')
import           Data.Hashable
import           Data.Text                      ( Text
                                                , pack
                                                )
import           GHC.Generics
-- import           Nix.String                          (StringContext)
import qualified Data.HashSet                  as S
import           Data.Binary                    ( Binary )
import           Data.Binary.Instances.UnorderedContainers

import           Data.Aeson
import           Data.Aeson.TH

-- | Atoms are values that evaluate to themselves. This means that
-- they appear in both the parsed AST (in the form of literals) and
-- the evaluated form.
data NAtom
  -- | An URI like @https://example.com@.
  = NURI Text
  -- | An integer. The c nix implementation currently only supports
  -- integers that fit in the range of 'Int64'.
  | NInt Integer
  -- | A floating point number
  | NFloat Float
  -- | Booleans.
  | NBool Bool
  -- | Null values. There's only one of this variant.
  | NNull
  | NString
    { nsContents :: !Text
    , nsContext :: !(S.HashSet StringContext)
    }
  deriving (Eq, Ord, Generic, Typeable, Data, Show, Read, NFData,
            Hashable)

-- instance Hashable NString
-- instance Hashable S.HashSet
-- instance Binary (S.HashSet a)

-- | A 'StringContext' ...
data StringContext =
  StringContext { scPath :: !Text
                , scFlavor :: !ContextFlavor
                } deriving (Eq, Ord, Show, Generic, Data, Read, NFData, Serialise, Binary, Hashable, ToJSON, FromJSON)

-- instance Hashable StringContext

-- | A 'ContextFlavor' describes the sum of possible derivations for string contexts
data ContextFlavor =
    DirectPath
  | AllOutputs
  | DerivationOutput !Text
  deriving (Show, Eq, Ord, Generic, Data, Read, NFData, Serialise, Binary, ToJSON, FromJSON)

instance Hashable ContextFlavor

#ifdef MIN_VERSION_serialise
instance Serialise NAtom
#endif

-- | Translate an atom into its nix representation.
atomText :: NAtom -> Text
atomText (NURI   t) = t
atomText (NInt   i) = pack (show i)
atomText (NFloat f) = pack (showNixFloat f)
  where
    showNixFloat x
      | x `mod'` 1 /= 0 = show x
      | otherwise       = show (truncate x :: Int)
atomText (NBool  b) = if b then "true" else "false"
atomText NNull      = "null"
