{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module EvalTests (tests) where

import Data.Fix

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.TH

import Nix.Eval
import Nix.Parser
import Nix.Expr

import Data.Monoid (Monoid(..))
import Prelude (String)

case_basic_sum :: Assertion
case_basic_sum = constantEqualStr "2" "1 + 1"

case_basic_function :: Assertion
case_basic_function = constantEqualStr "2" "(a: a) 2"

case_set_attr :: Assertion
case_set_attr = constantEqualStr "2" "{ a = 2; }.a"

case_function_set_arg :: Assertion
case_function_set_arg = constantEqualStr "2" "({ a }: 2) { a = 1; }"

case_function_set_two_arg :: Assertion
case_function_set_two_arg = constantEqualStr "2" "({ a, b ? 3 }: b - a) { a = 1; }"

-- case_function_set_two_arg_default_scope :: Assertion
-- case_function_set_two_arg_default_scope = constantEqualStr "2" "({ a, b ? a * 3 }: b - a) { a = 1; }"

tests :: TestTree
tests = $testGroupGenerator

-----------------------

constantEqual :: NExpr -> NExpr -> Assertion
constantEqual a b = do
    Fix (NVConstant a') <- evalExpr a mempty
    Fix (NVConstant b') <- evalExpr b mempty
    assertEqual "" a' b'

constantEqualStr :: String -> String -> Assertion
constantEqualStr a b =
  let Success a' = parseNixString a
      Success b' = parseNixString b
  in constantEqual a' b'
