{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# OPTIONS -Wno-orphans#-}
module PrettyParseTests  where

import Test.Tasty.QuickCheck hiding (Success, Failure)
import Test.Tasty
import Test.QuickCheck.Instances.Text ()
import Test.QuickCheck.Instances.Semigroup ()
import qualified Test.QuickCheck.Property as P

import Nix.Expr (NExpr, NExprF(..), NString(..), NUnaryOp(..), NBinaryOp(..)
                , Params(..), NKeyName(..), Antiquoted(..), Binding(..))
import Nix.Atoms
import Nix.Pretty
import Nix.Parser
import Generic.Random
import Data.Fix
import Data.Text (Text, pack, unpack)
import Text.Megaparsec (Pos, SourcePos, mkPos)
import Control.Monad
import Data.Algorithm.Diff
import Data.Algorithm.DiffOutput
import Data.Char

-- Instead of using the Generic arbitrary instance (which doesn't exist
-- anyway for Text), we use a different generator which just prints
-- sensible looking variable names
custom :: GenList '[Text]
custom = asciiText :@ Nil

asciiString :: Gen String
asciiString = do
  n <- choose (1, 15)
  replicateM n (elements ['a'..'z'])

asciiText :: Gen Text
asciiText = pack <$> asciiString

pcustom :: GenList '[Pos]
pcustom = (arbitrary) :@ Nil

-- | This generator generates selects one of the constructors uniformly
-- and also decreases the size of the generator by dividing by the
-- branching factor. This ensures sensible termination.
genArb :: (GArbitrary (Options 'Sized '[Text]) a, GUniformWeight a) =>  Gen a
genArb = genericArbitraryWith (setGenerators custom sizedOpts) uniform

-- Might want to replace this instance with a constant value
instance Arbitrary Pos where
  arbitrary = mkPos <$> (getSmall <$> arbitrary `suchThat` (> 0))

instance Arbitrary (f (Fix f)) => Arbitrary (Fix f) where
  arbitrary = genArb

instance Arbitrary f => Arbitrary (NString f) where
  arbitrary = genArb

instance Arbitrary SourcePos where
  arbitrary = genericArbitraryWith (setGenerators pcustom sizedOpts) uniform

instance Arbitrary f => Arbitrary (Binding f) where
  arbitrary = genArb

instance Arbitrary f => Arbitrary (NKeyName f) where
  arbitrary = genArb

instance Arbitrary f => Arbitrary (Params f) where
  arbitrary = genArb

instance Arbitrary NAtom where
  arbitrary = genArb

instance Arbitrary NUnaryOp where
  arbitrary = genArb

instance Arbitrary NBinaryOp where
  arbitrary = genArb

instance (Arbitrary f) => Arbitrary (Antiquoted Text f) where
  arbitrary = genArb

instance (Arbitrary f) => Arbitrary (Antiquoted (NString f) f) where
  arbitrary = genArb

-- This is written by hand so we can use `fairList` rather than
-- the normal list Arbitrary instance which makes the generator
-- terminate. The distribution is not scientifically chosen.
instance Arbitrary f => Arbitrary (NExprF f) where
  arbitrary =
    sized $ \n ->
        if n < 2
          then oneof [nConstant, nStr, nSym, nLiteralPath, nEnvPath ]
          else
            frequency
              [ (1, nConstant)
              , (1, nSym)
              , (4, resize (n `div` 3) nIf)
              , (10, nRecSet )
              , (20, nSet )
              , (5, nList )
              , (2, nUnary )
              , (2, resize (n `div` 3) nBinary )
              , (3, resize (n `div` 3) nSelect )
              , (20, resize (n `div` 2) nAbs )
              , (2, resize (n `div` 2) nHasAttr )
              , (10, resize (n `div` 2) nLet )
              , (10, resize (n `div` 2) nWith )
              , (1, resize (n `div` 2) nAssert)
              ]
   where
    nConstant = NConstant <$> arbitrary
    nStr = NStr <$> arbitrary
    nSym = NSym <$> asciiText
    nList = NList <$> fairList arbitrary
    nSet  = NSet <$> fairList arbitrary
    nRecSet = NRecSet <$> fairList arbitrary
    nLiteralPath = NLiteralPath <$> asciiString
    nEnvPath = NEnvPath <$> asciiString
    nUnary = NUnary <$> arbitrary <*> arbitrary
    nBinary = NBinary <$> arbitrary <*> arbitrary <*> arbitrary
    nSelect = NSelect <$> arbitrary <*> arbitrary <*> arbitrary
    nHasAttr = NHasAttr <$> arbitrary <*> arbitrary
    nAbs = NAbs <$> arbitrary <*> arbitrary
    nLet = NLet <$> arbitrary <*> arbitrary
    nIf = NIf <$> arbitrary <*> arbitrary <*> arbitrary
    nWith = NWith <$> arbitrary <*> arbitrary
    nAssert = NAssert <$> arbitrary <*> arbitrary

-- | Useful when there are recursive positions at each element of the list
-- as it divides the size by the length of the generated list.
fairList :: Gen a -> Gen [a]
fairList g = do
  s <- getSize
  k <- choose (0, s)
  -- Use max here to avoid dividing by zero when there is the empty list
  resize (s `div` (max 1 k)) $ vectorOf k g

-- | Test that pretty . parse . pretty == pretty
prop_prettyparse :: NExpr -> P.Result
prop_prettyparse p =
  case parse (pretty p) of
    Failure s -> P.rejected { P.reason = show s ++ show (pretty p)  }
    Success v ->
      let pp = normalise (unpack (pretty p))
          pv = normalise (unpack (pretty v))
      in (P.liftBool (pp == pv)) { P.reason = "Bad parse:" ++ pp ++ pv ++ ppDiff (diff pp pv) ++ show p ++ show v}
  where
    pretty = pack . show . prettyNix
    parse = parseNixText

    normalise = unlines . map (reverse . dropWhile isSpace . reverse) . lines

diff :: String -> String -> [Diff [String]]
diff s1 s2 = getDiff (map (:[]) (lines s1)) (map (:[]) (lines s2))

tests :: TestTree
tests = testProperty "Pretty Parse Property" prop_prettyparse
