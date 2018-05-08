{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS -Wno-orphans#-}

module PrettyParseTests  where

import           Control.Monad
import           Data.Algorithm.Diff
import           Data.Algorithm.DiffOutput
import           Data.Char
import           Data.Fix
import qualified Data.List.NonEmpty as NE
import           Data.Text (Text, pack)
import qualified Data.Text as Text
import           Generic.Random
import           Nix.Atoms
import           Nix.Expr
import           Nix.Parser
import           Nix.Pretty
import           Test.QuickCheck.Instances.Semigroup ()
import           Test.QuickCheck.Instances.Text ()
import qualified Test.QuickCheck.Property as P
import           Test.Tasty
import           Test.Tasty.QuickCheck hiding (Success, Failure)
import           Text.Megaparsec (Pos, SourcePos, mkPos)
import           Text.PrettyPrint.ANSI.Leijen ((</>), text)
import qualified Text.PrettyPrint.ANSI.Leijen as P
import qualified Text.Show.Pretty as PS

-- Instead of using the Generic arbitrary instance (which doesn't exist anyway
-- for Text), we use a different generator which just prints sensible looking
-- variable names
custom :: GenList '[Text]
custom = asciiText :@ Nil

asciiString :: Gen String
asciiString = do
  n <- choose (1, 15)
  replicateM n (elements ['a'..'z'])

asciiText :: Gen Text
asciiText = pack <$> asciiString

pcustom :: GenList '[Pos]
pcustom = arbitrary :@ Nil

-- | This generator generates selects one of the constructors uniformly and
--   also decreases the size of the generator by dividing by the branching
--   factor. This ensures sensible termination.
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
  arbitrary = oneof [ DynamicKey <$> arbitrary
                    , StaticKey <$> asciiText <*> arbitrary ]

instance Arbitrary f => Arbitrary (Params f) where
  arbitrary =
      oneof [ Param    <$> asciiText
            , ParamSet <$> listOf ((,) <$> asciiText <*> arbitrary) <*> arbitrary
                       <*> oneof [pure Nothing, Just <$> asciiText]
            ]

instance Arbitrary NAtom where
  arbitrary =
      oneof [ NInt   <$> arbitrary `suchThat` (>= 0)
            , NFloat <$> arbitrary `suchThat` (>= 0)
            , NBool  <$> arbitrary
            , pure NNull ]

instance Arbitrary NUnaryOp where
  arbitrary = genArb

instance Arbitrary NBinaryOp where
  arbitrary = genArb

instance (Arbitrary f) => Arbitrary (Antiquoted Text f) where
  arbitrary = genArb

instance (Arbitrary f) => Arbitrary (Antiquoted (NString f) f) where
  arbitrary = genArb

-- This is written by hand so we can use `fairList` rather than the normal
-- list Arbitrary instance which makes the generator terminate. The
-- distribution is not scientifically chosen.
instance Arbitrary f => Arbitrary (NExprF f) where
  arbitrary =
    sized $ \n ->
        if n < 2
          then oneof [genConstant, genStr, genSym, genLiteralPath, genEnvPath ]
          else
            frequency
              [ ( 1, genConstant)
              , ( 1, genSym)
              , ( 4, resize (n `div` 3) genIf)
              , (10, genRecSet )
              , (20, genSet )
              , ( 5, genList )
              , ( 2, genUnary )
              , ( 2, resize (n `div` 3) genBinary )
              , ( 3, resize (n `div` 3) genSelect )
              , (20, resize (n `div` 2) genAbs )
              , ( 2, resize (n `div` 2) genHasAttr )
              , (10, resize (n `div` 2) genLet )
              , (10, resize (n `div` 2) genWith )
              , ( 1, resize (n `div` 2) genAssert)
              ]
   where
    genConstant    = NConstant    <$> arbitrary
    genStr         = NStr         <$> arbitrary
    genSym         = NSym         <$> asciiText
    genList        = NList        <$> fairList arbitrary
    genSet         = NSet         <$> fairList arbitrary
    genRecSet      = NRecSet      <$> fairList arbitrary
    genLiteralPath = NLiteralPath . ("./" ++) <$> asciiString
    genEnvPath     = NEnvPath     <$> asciiString
    genUnary       = NUnary       <$> arbitrary <*> arbitrary
    genBinary      = NBinary      <$> arbitrary <*> arbitrary <*> arbitrary
    genSelect      = NSelect      <$> arbitrary <*> arbitrary <*> arbitrary
    genHasAttr     = NHasAttr     <$> arbitrary <*> arbitrary
    genAbs         = NAbs         <$> arbitrary <*> arbitrary
    genLet         = NLet         <$> fairList arbitrary <*> arbitrary
    genIf          = NIf          <$> arbitrary <*> arbitrary <*> arbitrary
    genWith        = NWith        <$> arbitrary <*> arbitrary
    genAssert      = NAssert      <$> arbitrary <*> arbitrary

-- | Useful when there are recursive positions at each element of the list as
--   it divides the size by the length of the generated list.
fairList :: Gen a -> Gen [a]
fairList g = do
  s <- getSize
  k <- choose (0, s)
  -- Use max here to avoid dividing by zero when there is the empty list
  resize (s `div` max 1 k) $ vectorOf k g

equivUpToNormalization :: NExpr -> NExpr -> Bool
equivUpToNormalization x y = normalize x == normalize y

normalize :: NExpr -> NExpr
normalize = cata $ \case
  NConstant (NInt n)   | n < 0 -> Fix (NUnary NNeg (Fix (NConstant (NInt (negate n)))))
  NConstant (NFloat n) | n < 0 -> Fix (NUnary NNeg (Fix (NConstant (NFloat (negate n)))))

  NSet binds      -> Fix (NSet (map normBinding binds))
  NRecSet binds   -> Fix (NRecSet (map normBinding binds))
  NLet binds r    -> Fix (NLet (map normBinding binds) r)

  NAbs params r   -> Fix (NAbs (normParams params) r)

  r               -> Fix r

 where
  normBinding (NamedVar path r) = NamedVar (NE.map normKey path) r
  normBinding (Inherit mr names) = Inherit mr (map normKey names)

  normKey (DynamicKey quoted) = DynamicKey (normAntiquotedString quoted)
  normKey (StaticKey name _)  = StaticKey name Nothing

  normAntiquotedString :: Antiquoted (NString NExpr) NExpr
                       -> Antiquoted (NString NExpr) NExpr
  normAntiquotedString (Plain (DoubleQuoted [EscapedNewline])) =
      EscapedNewline
  normAntiquotedString (Plain (DoubleQuoted strs)) =
      let strs' = map normAntiquotedText strs
      in if strs == strs'
         then Plain (DoubleQuoted strs)
         else normAntiquotedString (Plain (DoubleQuoted strs'))
  normAntiquotedString r = r

  normAntiquotedText :: Antiquoted Text NExpr -> Antiquoted Text NExpr
  normAntiquotedText (Plain "\n")   = EscapedNewline
  normAntiquotedText (Plain "''\n") = EscapedNewline
  normAntiquotedText r = r

  normParams (ParamSet binds var (Just "")) = ParamSet binds var Nothing
  normParams r = r

-- | Test that parse . pretty == id up to attribute position information.
prop_prettyparse :: NExpr -> P.Result
prop_prettyparse p =
  let prog = show (pretty p)
  in case parse (pack prog) of
    Failure s -> P.rejected
        { P.reason = show $
            text "Parse failed:" </> text (show s)
              P.<$> P.indent 2 (pretty p) }
    Success v
        | equivUpToNormalization p v -> P.succeeded
        | otherwise ->
          let pp = normalise prog
              pv = normalise (show (pretty v))
          in (P.liftBool (pp == pv))
            { P.reason = show $
                      text "----------------------------------------"
                P.<$> text "Expr before:" P.<$> P.indent 2 (text (PS.ppShow p))
                P.<$> text "----------------------------------------"
                P.<$> text "Expr after:"  P.<$> P.indent 2 (text (PS.ppShow v))
                P.<$> text "----------------------------------------"
                P.<$> text "Pretty before:" P.<$> P.indent 2 (text prog)
                P.<$> text "----------------------------------------"
                P.<$> text "Pretty after:"  P.<$> P.indent 2 (pretty v)
                P.<$> text "----------------------------------------"
                P.<$> text "Normalised before:" P.<$> P.indent 2 (text pp)
                P.<$> text "----------------------------------------"
                P.<$> text "Normalised after:"  P.<$> P.indent 2 (text pv)
                P.<$> text "========================================"
                P.<$> text "Normalised diff:"
                P.<$> text (ppDiff (diff pp pv))
                P.<$> text "========================================"
            }
  where
    pretty = prettyNix
    parse  = parseNixText

    normalise = unlines . map (reverse . dropWhile isSpace . reverse) . lines

    diff :: String -> String -> [Diff [String]]
    diff s1 s2 = getDiff (map (:[]) (lines s1)) (map (:[]) (lines s2))

tests :: Int -> TestTree
tests n = testProperty "Pretty/Parse Property" $
    withMaxSuccess n prop_prettyparse
