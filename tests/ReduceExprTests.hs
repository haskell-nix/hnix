{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module ReduceExprTests (tests) where
import           Data.Fix
import           Test.Tasty
import           Test.Tasty.HUnit

import           Nix.Atoms
import           Nix.Expr.Types
import           Nix.Expr.Types.Annotated
import           Nix.Parser
import           Nix.Reduce                     ( reduceExpr )


tests :: TestTree
tests = testGroup
  "Expr Reductions"
  [ testCase "Non nested NSelect on set should be reduced"
    $ cmpReduceResult selectBasic selectBasicExpect
  , testCase "Nested NSelect on set should be reduced"
    $ cmpReduceResult selectNested selectNestedExpect
  , testCase "Non nested NSelect with incorrect attrpath shouldn't be reduced"
    $ shouldntReduce selectIncorrectAttrPath
  , testCase "Nested NSelect with incorrect attrpath shouldn't be reduced"
    $ shouldntReduce selectNestedIncorrectAttrPath
  ]

assertSucc :: Result a -> IO a
assertSucc (Success a) = pure a
assertSucc (Failure d) = assertFailure $ show d

cmpReduceResult :: Result NExprLoc -> NExpr -> Assertion
cmpReduceResult r e = do
  r <- assertSucc r
  r <- stripAnnotation <$> reduceExpr mempty r
  r @?= e

shouldntReduce :: Result NExprLoc -> Assertion
shouldntReduce r = do
  r        <- assertSucc r
  rReduced <- reduceExpr mempty r
  r @?= rReduced

selectBasic :: Result NExprLoc
selectBasic = parseNixTextLoc "{b=2;a=42;}.a"

selectBasicExpect :: NExpr
selectBasicExpect = Fix . NConstant $ NInt 42

selectNested :: Result NExprLoc
selectNested = parseNixTextLoc "{a={b=2;a=42;};b={a=2;};}.a.a"

selectNestedExpect :: NExpr
selectNestedExpect = Fix . NConstant $ NInt 42

selectIncorrectAttrPath :: Result NExprLoc
selectIncorrectAttrPath = parseNixTextLoc "{a=42;}.b"

selectNestedIncorrectAttrPath :: Result NExprLoc
selectNestedIncorrectAttrPath = parseNixTextLoc "{a={a=42;};}.a.b"
