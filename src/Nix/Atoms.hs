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
import           Data.Fixed                     ( mod' )
import           Data.Hashable
import           Data.Text                      ( Text
                                                , pack
                                                )
import           GHC.Generics
import           Data.Binary                    ( Binary )
import           Data.Aeson.Types               ( FromJSON, ToJSON )

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
  -- | Booleans. @false@ or @true@.
  | NBool Bool
  -- | Null values. There's only one of this variant: @null@.
  | NNull
  deriving (Eq, Ord, Generic, Typeable, Data, Show, Read, NFData,
            Hashable)

#ifdef MIN_VERSION_serialise
instance Serialise NAtom
#endif

instance Binary NAtom
instance ToJSON NAtom
instance FromJSON NAtom

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
