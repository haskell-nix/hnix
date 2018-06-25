{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module ReduceExprTests (tests) where
import Data.Fix
import Test.Tasty
import Test.Tasty.HUnit

import Nix.Atoms
import Nix.Expr.Types
import Nix.Expr.Types.Annotated
import Nix.Reduce (reduceExpr)
import Nix.Parser


tests :: TestTree
tests = testGroup "Expr Reductions"
    [ testCase "Non nested NSelect on set should be reduced" $ 
        cmpReduceResult nonNestedSelect nonNestedSelectExpect 
    ]

cmpReduceResult :: Result NExprLoc -> NExpr -> Assertion 
cmpReduceResult r e = do
    r <- assertSucc r
    r <- stripAnnotation <$> reduceExpr Nothing r
    r @?= e
    where
        assertSucc (Success a) = pure a
        assertSucc (Failure d) = assertFailure $ show d

nonNestedSelect :: Result NExprLoc
nonNestedSelect = parseNixTextLoc "{a=42;}.a"

nonNestedSelectExpect :: NExpr
nonNestedSelectExpect = Fix . NConstant $ NInt 42
