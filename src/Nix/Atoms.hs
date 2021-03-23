{-# LANGUAGE CPP            #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Nix.Atoms where

#ifdef MIN_VERSION_serialise
import           Codec.Serialise                ( Serialise )
#endif

import           Data.Data                      ( Data)
import           Data.Fixed                     ( mod' )
import           Data.Binary                    ( Binary )
import           Data.Aeson.Types               ( FromJSON
                                                , ToJSON
                                                )

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

#ifdef MIN_VERSION_serialise
instance Serialise NAtom
#endif

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
