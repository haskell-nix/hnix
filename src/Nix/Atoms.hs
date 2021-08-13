{-# language CPP            #-}
{-# language DeriveAnyClass #-}

module Nix.Atoms where

import           Codec.Serialise                ( Serialise )

import           Data.Data                      ( Data)
import           Data.Fixed                     ( mod' )
import           Data.Binary                    ( Binary )
import           Data.Aeson.Types               ( FromJSON
                                                , ToJSON
                                                )
--  2021-08-01: NOTE: Check the order efficience of NAtom constructors.

-- | Atoms are values that evaluate to themselves.
-- In other words - this is a constructors that are literals in Nix.
-- This means that
-- they appear in both the parsed AST (in the form of literals) and
-- the evaluated form as themselves.
-- Once HNix parsed or evaluated into atom - that is a literal
-- further after, for any further evaluation it is in all cases stays
-- constantly itself.
-- "atom", Ancient Greek \( atomos \) - "indivisible" particle,
-- indivisible expression.
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
  deriving
    ( Eq
    , Ord
    , Generic
    , Typeable
    , Data
    , Show
    , Read
    , NFData
    , Hashable
    )

instance Serialise NAtom

instance Binary NAtom
instance ToJSON NAtom
instance FromJSON NAtom

-- | Translate an atom into its Nix representation.
atomText :: NAtom -> Text
atomText (NURI   t) = t
atomText (NInt   i) = show i
atomText (NFloat f) = showNixFloat f
 where
  showNixFloat :: Float -> Text
  showNixFloat x =
    bool
      (show x)
      (show (truncate x :: Int))
      (x `mod'` 1 == 0)
atomText (NBool  b) = if b then "true" else "false"
atomText NNull      = "null"
