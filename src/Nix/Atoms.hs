{-# language CPP               #-}
{-# language DeriveAnyClass    #-}

module Nix.Atoms
  ( NAtom(..)
  , atomText
  -- Re-export Int64 for convenience
  , Int64
  -- Checked arithmetic operations (throw on overflow)
  , checkedAdd
  , checkedSub
  , checkedMul
  , checkedDiv
  , checkedNeg
  ) where

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
  -- | An integer. The Nix implementation uses checked 64-bit integers
  -- that throw an error on overflow.
  | NInt Int64
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

-- | Checked arithmetic operations for Int64 that match Nix's behavior.
-- These throw an error on overflow rather than wrapping.

-- | Checked addition. Throws on overflow.
checkedAdd :: Int64 -> Int64 -> Either String Int64
checkedAdd x y
  | y > 0 && x > maxBound - y = Left $ "integer overflow in adding " <> show x <> " + " <> show y
  | y < 0 && x < minBound - y = Left $ "integer overflow in adding " <> show x <> " + " <> show y
  | otherwise = Right (x + y)

-- | Checked subtraction. Throws on overflow.
checkedSub :: Int64 -> Int64 -> Either String Int64
checkedSub x y
  | y < 0 && x > maxBound + y = Left $ "integer overflow in subtracting " <> show x <> " - " <> show y
  | y > 0 && x < minBound + y = Left $ "integer overflow in subtracting " <> show x <> " - " <> show y
  | otherwise = Right (x - y)

-- | Checked multiplication. Throws on overflow.
checkedMul :: Int64 -> Int64 -> Either String Int64
checkedMul x y
  | x == 0 || y == 0 = Right 0
  | x == -1 && y == minBound = Left $ "integer overflow in multiplying " <> show x <> " * " <> show y
  | y == -1 && x == minBound = Left $ "integer overflow in multiplying " <> show x <> " * " <> show y
  | x > 0 && y > 0 && x > maxBound `div` y = Left $ "integer overflow in multiplying " <> show x <> " * " <> show y
  | x > 0 && y < 0 && y < minBound `div` x = Left $ "integer overflow in multiplying " <> show x <> " * " <> show y
  | x < 0 && y > 0 && x < minBound `div` y = Left $ "integer overflow in multiplying " <> show x <> " * " <> show y
  | x < 0 && y < 0 && x < maxBound `div` y = Left $ "integer overflow in multiplying " <> show x <> " * " <> show y
  | otherwise = Right (x * y)

-- | Checked integer division. Returns floor of division (matching Nix).
-- Throws on division by zero.
checkedDiv :: Int64 -> Int64 -> Either String Int64
checkedDiv _ 0 = Left "division by zero"
checkedDiv x y = Right (x `div` y)

-- | Checked negation. Throws on overflow (negating minBound).
checkedNeg :: Int64 -> Either String Int64
checkedNeg x
  | x == minBound = Left $ "integer overflow in negating " <> show x
  | otherwise = Right (negate x)
