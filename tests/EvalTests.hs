{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -Wno-missing-signatures -Wno-orphans #-}

module EvalTests (tests) where

import Data.String.Interpolate
import Nix
import Nix.Expr
import Nix.Monad
import Nix.Parser
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.TH

case_basic_sum =
    constantEqualStr "2" "1 + 1"

case_basic_function =
    constantEqualStr "2" "(a: a) 2"

case_set_attr =
    constantEqualStr "2" "{ a = 2; }.a"

case_function_set_arg =
    constantEqualStr "2" "({ a }: 2) { a = 1; }"

case_function_set_two_arg =
    constantEqualStr "2" "({ a, b ? 3 }: b - a) { a = 1; }"

case_function_set_two_arg_default_scope =
    constantEqualStr "2" "({ x ? 1, y ? x * 3 }: y - x) {}"

case_function_default_env =
    constantEqualStr "2" "let default = 2; in ({ a ? default }: a) {}"

case_function_definition_uses_environment =
    constantEqualStr "3" "let f = (let a=1; in x: x+a); in f 2"

case_function_atpattern =
    constantEqualStr "2" "(({a}@attrs:attrs) {a=2;}).a"

case_function_ellipsis =
    constantEqualStr "2" "(({a, ...}@attrs:attrs) {a=0; b=2;}).b"

case_function_default_value_in_atpattern =
    constantEqualStr "2" "({a ? 2}@attrs:attrs.a) {}"

case_function_arg_shadowing =
    constantEqualStr "6" "(y: y: x: x: x + y) 1 2 3 4"

case_function_recursive_args =
    constantEqualStr "2" "({ x ? 1, y ? x * 3}: y - x) {}"

case_function_recursive_sets =
    constantEqualStr "[ [ 6 4 100 ] 4 ]" [i|
        let x = rec {

          y = 2;
          z = { w = 4; };
          v = rec {
            u = 6;
            t = [ u z.w s ];
          };

        }; s = 100; in [ x.v.t x.z.w ]
    |]

case_nested_with =
    constantEqualStr "2" "with { x = 1; }; with { x = 2; }; x"

-----------------------

tests :: TestTree
tests = $testGroupGenerator

instance (Show r, Eq r) => Eq (NValueF m r) where
    NVConstant x == NVConstant y = x == y
    NVList x == NVList y = and (zipWith (==) x y)
    x == y = error $ "Need to add comparison for values: "
                 ++ show x ++ " == " ++ show y

constantEqual :: NExprLoc -> NExprLoc -> Assertion
constantEqual a b = do
    -- putStrLn =<< lint (stripAnnotation a)
    a' <- tracingEvalLoc Nothing a
    -- putStrLn =<< lint (stripAnnotation b)
    b' <- tracingEvalLoc Nothing b
    assertEqual "" a' b'

constantEqualStr :: String -> String -> Assertion
constantEqualStr a b =
  let Success a' = parseNixStringLoc a
      Success b' = parseNixStringLoc b
  in constantEqual a' b'
