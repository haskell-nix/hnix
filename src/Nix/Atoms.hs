{-# language CPP               #-}
{-# language DeriveAnyClass    #-}

module Nix.Atoms where

import           Nix.Prelude
import           Codec.Serialise                ( Serialise )

import           Data.Data                      ( Data )
import           Data.Fixed                     ( mod' )
import           Numeric                        ( showEFloat, showFFloat )
import           Data.List                      ( dropWhileEnd )
import           Data.Binary                    ( Binary )
import           Data.Aeson.Types               ( FromJSON
                                                , ToJSON
                                                )
--  2021-08-01: NOTE: Check the order effectiveness of NAtom constructors.

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
  | NFloat Double
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
  -- | Format a Double to match Nix's output format:
  -- 1. Whole numbers < 1e6 are shown as integers (no decimal point)
  -- 2. |x| >= 1e6 or (|x| < 1e-4 and x /= 0) uses scientific notation
  -- 3. Otherwise uses decimal notation with 6 significant figures
  showNixFloat :: Double -> Text
  showNixFloat x
    -- Whole numbers less than 1e6 are displayed as integers
    | x `mod'` 1 == 0 && abs x < 1e6 = show (truncate x :: Integer)
    -- Large values (>= 1e6) or very small values (< 1e-4) use scientific notation
    | abs x >= 1e6 || (abs x < 1e-4 && x /= 0) = fromString $ formatScientific x
    -- Normal range uses decimal notation
    | otherwise = fromString $ formatDecimal x

  -- Format in scientific notation with 6 significant figures
  formatScientific :: Double -> String
  formatScientific x =
    let raw = showEFloat (Just 5) x ""  -- 5 decimals = 6 sig figs
    in cleanupScientific raw

  -- Format in decimal notation with up to 6 significant figures
  formatDecimal :: Double -> String
  formatDecimal x =
    -- Calculate decimal places needed for ~6 significant figures
    let absX = abs x
        -- Number of digits before decimal point (minimum 1)
        digitsBeforeDecimal = max 1 $ floor (logBase 10 absX) + 1
        -- Decimal places to show (6 sig figs - digits before decimal)
        decimalPlaces = max 0 $ 6 - digitsBeforeDecimal
        raw = showFFloat (Just decimalPlaces) x ""
    in cleanupDecimal raw

  -- Clean up scientific notation: remove trailing zeros, format exponent
  cleanupScientific :: String -> String
  cleanupScientific s =
    case break (== 'e') s of
      (mantissa, expo) ->
        cleanupMantissa mantissa <> formatExponent expo

  -- Clean up decimal notation: remove trailing zeros
  cleanupDecimal :: String -> String
  cleanupDecimal = cleanupMantissa

  cleanupMantissa :: String -> String
  cleanupMantissa s =
    case break (== '.') s of
      (whole, '.' : frac) ->
        let trimmed = dropWhileEnd (== '0') frac
        in if null trimmed
           then whole
           else whole <> "." <> trimmed
      _ -> s

  formatExponent :: String -> String
  formatExponent "" = ""
  formatExponent ('e' : rest) =
    case rest of
      '-' : num -> "e-" <> padExponent num
      '+' : num -> "e+" <> padExponent num
      num       -> "e+" <> padExponent num
  formatExponent s = s

  -- Ensure exponent has at least 2 digits
  padExponent :: String -> String
  padExponent [c] = '0' : [c]
  padExponent s   = s
atomText (NBool  b) = if b then "true" else "false"
atomText NNull      = "null"
