{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-missing-signatures -Wno-orphans #-}

module EvalTests (tests, genEvalCompareTests) where

import           Data.String.Interpolate.IsString
import           Data.Text (Text)
import           Nix
import           Nix.Expr
import           Nix.Parser
import           Nix.Value
import qualified System.Directory as D
import           System.FilePath
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.TH
import           TestCommon

case_basic_sum =
    constantEqualText "2" "1 + 1"

case_basic_function =
    constantEqualText "2" "(a: a) 2"

case_set_attr =
    constantEqualText "2" "{ a = 2; }.a"

case_function_set_arg =
    constantEqualText "2" "({ a }: 2) { a = 1; }"

case_function_set_two_arg =
    constantEqualText "2" "({ a, b ? 3 }: b - a) { a = 1; }"

case_function_set_two_arg_default_scope =
    constantEqualText "2" "({ x ? 1, y ? x * 3 }: y - x) {}"

case_function_default_env =
    constantEqualText "2" "let default = 2; in ({ a ? default }: a) {}"

case_function_definition_uses_environment =
    constantEqualText "3" "let f = (let a=1; in x: x+a); in f 2"

case_function_atpattern =
    constantEqualText "2" "(({a}@attrs:attrs) {a=2;}).a"

case_function_ellipsis =
    constantEqualText "2" "(({a, ...}@attrs:attrs) {a=0; b=2;}).b"

case_function_default_value_not_in_atpattern =
    constantEqualText "false" "({a ? 2}@attrs: attrs ? a) {}"

case_function_arg_shadowing =
    constantEqualText "6" "(y: y: x: x: x + y) 1 2 3 4"

case_function_recursive_args =
    constantEqualText "2" "({ x ? 1, y ? x * 3}: y - x) {}"

case_function_recursive_sets =
    constantEqualText "[ [ 6 4 100 ] 4 ]" [i|
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
    constantEqualText "2" "with { x = 1; }; with { x = 2; }; x"

case_match_failure_null = assertEvalMatchesNix "builtins.match \"ab\" \"abc\""

-----------------------

tests :: TestTree
tests = $testGroupGenerator

genEvalCompareTests = do
    files <- filter (\x -> takeExtension x == "nix") <$> D.listDirectory testDir
    return $ testGroup "Eval comparison tests" $ map mkTestCase files
  where
    testDir = "tests/eval-compare"
    mkTestCase f = testCase f $ assertEvalFileMatchesNix (testDir </> f)

instance (Show r, Show (NValueF m r), Eq r) => Eq (NValueF m r) where
    NVConstant x == NVConstant y = x == y
    NVList x == NVList y = and (zipWith (==) x y)
    x == y = error $ "Need to add comparison for values: "
                 ++ show x ++ " == " ++ show y

constantEqual :: NExprLoc -> NExprLoc -> Assertion
constantEqual a b = do
    -- putStrLn =<< lint (stripAnnotation a)
    a' <- evalLoc Nothing [] a
    -- putStrLn =<< lint (stripAnnotation b)
    b' <- evalLoc Nothing [] b
    assertEqual "" a' b'

constantEqualText :: Text -> Text -> Assertion
constantEqualText a b =
  let Success a' = parseNixTextLoc a
      Success b' = parseNixTextLoc b
  in constantEqual a' b'
