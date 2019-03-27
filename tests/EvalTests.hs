{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-missing-signatures -Wno-orphans #-}

module EvalTests (tests, genEvalCompareTests) where

import           Control.Applicative ((<|>))
import           Control.Monad (when, unless)
import           Control.Monad.Catch
import           Control.Monad.IO.Class
-- import qualified Data.HashMap.Lazy as M
import           Data.List ((\\))
import           Data.Maybe (isJust)
import qualified Data.Set as S
import           Data.String.Interpolate.IsString
import           Data.Text (Text)
import           Data.Time
import           Nix
import           Nix.Standard
import           Nix.TH
import           Nix.Value.Equal
import qualified System.Directory as D
import           System.Environment
import           System.FilePath
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.TH
import           TestCommon

case_basic_sum =
    constantEqualText "2" "1 + 1"

case_basic_div =
    constantEqualText "3" "builtins.div 6 2"

case_zero_div = do
  assertNixEvalThrows "builtins.div 1 0"
  assertNixEvalThrows "builtins.div 1.0 0"
  assertNixEvalThrows "builtins.div 1 0.0"
  assertNixEvalThrows "builtins.div 1.0 0.0"

case_bit_ops = do
    -- mic92 (2018-08-20): change to constantEqualText,
    -- when hnix's nix fork supports bitAnd/bitOr/bitXor
    constantEqualText' "0" "builtins.bitAnd 1 0"
    constantEqualText' "1" "builtins.bitOr 1 1"
    constantEqualText' "3" "builtins.bitXor 1 2"

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
    -- jww (2018-05-09): This should be constantEqualText
    constantEqualText' "2" "(({a}@attrs:attrs) {a=2;}).a"

case_function_ellipsis =
    -- jww (2018-05-09): This should be constantEqualText
    constantEqualText' "2" "(({a, ...}@attrs:attrs) {a=0; b=2;}).b"

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

case_with_strictness =
    constantEqualText "5" "let x = with x; with { a = 5; }; a; in x"

case_match_failure_null =
    constantEqualText "null" "builtins.match \"ab\" \"abc\""

case_find_file_success_no_prefix =
    constantEqualText "./tests/files/findFile.nix"
                      "builtins.findFile [{ path=\"./tests/files\"; prefix=\"\"; }] \"findFile.nix\""

case_find_file_success_with_prefix =
    constantEqualText "./tests/files/findFile.nix"
                      "builtins.findFile [{ path=\"./tests/files\"; prefix=\"nix\"; }] \"nix/findFile.nix\""

case_find_file_success_folder =
    constantEqualText "./tests/files"
                      "builtins.findFile [{ path=\"./tests\"; prefix=\"\"; }] \"files\""

case_find_file_failure_not_found =
    assertNixEvalThrows "builtins.findFile [{ path=\"./tests/files\"; prefix=\"\"; }] \"not_found.nix\""

case_find_file_failure_invalid_arg_1 =
    assertNixEvalThrows "builtins.findFile 1 \"files\""

case_find_file_failure_invalid_arg_2 =
    assertNixEvalThrows "builtins.findFile [{ path=\"./tests/files\"; prefix=\"\"; }] 2"

case_find_file_failure_invalid_arg_no_path =
    assertNixEvalThrows "builtins.findFile [{ prefix=\"\"; }] \"files\""

case_infinite_recursion =
    assertNixEvalThrows "let foo = a: bar a; bar = a: foo a; in foo 3"

case_inherit_in_rec_set =
    constantEqualText "1" "let x = 1; in (rec { inherit x; }).x"

case_lang_version =
    constantEqualText "5" "builtins.langVersion"

case_rec_set_attr_path_simpl =
    constantEqualText "123" [i|
      let x = rec {
        foo.number = 123;
        foo.function = y: foo.number;
      }; in x.foo.function 1
    |]

case_inherit_from_set_has_no_scope =
    constantEqualText' "false" [i|
      (builtins.tryEval (
        let x = 1;
            y = { z = 2; };
        in { inherit (y) x; }.x
      )).success
    |]

-- github/orblivion (2018-08-05): Adding these failing tests so we fix this feature

-- case_overrides =
--     constantEqualText' "2" [i|
--       let
--
--         overrides = { a = 2; };
--
--       in (rec {
--         __overrides = overrides;
--         x = a;
--         a = 1;
--       }.__overrides.a)
--     |]

-- case_inherit_overrides =
--     constantEqualText' "2" [i|
--       let
--
--         __overrides = { a = 2; };
--
--       in (rec {
--         inherit __overrides;
--         x = a;
--         a = 1;
--       }.__overrides.a)
--     |]

case_unsafegetattrpos1 =
    constantEqualText "[ 6 20 ]" [i|
      let e = 1;
          f = 1;
          t = {};
          s = {
            inherit t e f;
            a = 1;
            "b" = 2;
            c.d = 3;
          };
          p = builtins.unsafeGetAttrPos "e" s; in
      [ p.line p.column ]
    |]

case_unsafegetattrpos2 =
    constantEqualText "[ 6 20 ]" [i|
      let e = 1;
          f = 1;
          t = {};
          s = {
            inherit t e f;
            a = 1;
            "b" = 2;
            c.d = 3;
          };
          p = builtins.unsafeGetAttrPos "f" s; in
      [ p.line p.column ]
    |]

case_unsafegetattrpos3 =
    constantEqualText "[ 7 13 ]" [i|
      let e = 1;
          f = 1;
          t = {};
          s = {
            inherit t e f;
            a = 1;
            "b" = 2;
            c.d = 3;
          };
          p = builtins.unsafeGetAttrPos "a" s; in
      [ p.line p.column ]
    |]

case_unsafegetattrpos4 =
    constantEqualText "[ 8 13 ]" [i|
      let e = 1;
          f = 1;
          t = {};
          s = {
            inherit t e f;
            a = 1;
            "b" = 2;
            c.d = 3;
          };
          p = builtins.unsafeGetAttrPos "b" s; in
      [ p.line p.column ]
    |]

-- jww (2018-05-09): These two are failing but they shouldn't be

-- case_unsafegetattrpos5 =
--     constantEqualText "[ 7 13 ]" [i|
--       let e = 1;
--           f = 1;
--           t = {};
--           s = {
--             inherit t e f;
--             a = 1;
--             "b" = 2;
--             c.d = 3;
--           };
--           p = builtins.unsafeGetAttrPos "c.d" s; in
--       [ p.line p.column ]
--     |]

-- case_unsafegetattrpos6 =
--     constantEqualText "[ 7 13 ]" [i|
--       let e = 1;
--           f = 1;
--           t = {};
--           s = {
--             inherit t e f;
--             a = 1;
--             "b" = 2;
--             c.d = 3;
--           };
--           p = builtins.unsafeGetAttrPos "d" s; in
--       [ p.line p.column ]
--     |]

case_fixed_points =
    constantEqualText [i|[
  {
    foobar = "foobar";
    foo = "foo";
    bar = "bar";
  }
  {
    foobar = "foo + bar";
    foo = "foo + ";
    bar = "bar";
  }
]|] [i|
    let
      fix = f: let x = f x; in x;
      extends = f: rattrs: self:
        let super = rattrs self; in super // f self super;
      f = self: { foo = "foo";
                  bar = "bar";
                  foobar = self.foo + self.bar; };
      g = self: super: { foo = super.foo + " + "; };
    in [ (fix f) (fix (extends g f)) ]
|]

case_fixed_points_and_fold =
    constantEqualText [i|[ {} {} ]|] [i|
let
  extends = f: rattrs: self:
    let super = rattrs self; in super // f self super;
  flip = f: a: b: f b a;
  toFixFold = builtins.foldl' (flip extends) (self: {}) ([(self: super: {})]);
  toFix = extends (self: super: {}) (self: {});
  fix = f: let x = f x; in x;
in [ (fix toFixFold) (fix toFix) ]
|]

case_fixed_points_attrsets =
    constantEqualText "{ x = { y = { z = 100; }; z = { y = 100; }; }; }" [i|
      let fix = f: let x = f x; in x;
          f = self: { x.z.y = 100; x.y.z = self.x.z.y; };
      in fix f
    |]

-- case_function_equals1 =
--     constantEqualText "true" "{f = x: x;} == {f = x: x;}"

-- case_function_equals2 =
--     constantEqualText "true" "[(x: x)] == [(x: x)]"

case_function_equals3 =
    constantEqualText "false" "(let a = (x: x); in a == a)"

case_function_equals4 =
    constantEqualText "true" "(let a = {f = x: x;}; in a == a)"

case_function_equals5 =
    constantEqualText "true" "(let a = [(x: x)]; in a == a)"

case_directory_pathexists =
    constantEqualText "false" "builtins.pathExists \"/var/empty/invalid-directory\""

-- jww (2018-05-02): This constantly changes!
-- case_placeholder =
--   constantEqualText
--       "\"/1rz4g4znpzjwh1xymhjpm42vipw92pr73vdgl6xs1hycac8kf2n9\""
--       "builtins.placeholder \"out\""

case_rec_path_attr =
    constantEqualText "10"
        "let src = 10; x = rec { passthru.src = src; }; in x.passthru.src"

case_mapattrs_builtin =
    constantEqualText' "{ a = \"afoo\"; b = \"bbar\"; }" [i|
      (builtins.mapAttrs (x: y: x + y) {
        a = "foo";
        b = "bar";
      })
    |]

-- Regression test for #373
case_regression_373 :: Assertion
case_regression_373 = do
  freeVarsEqual "{ inherit a; }" ["a"]
  freeVarsEqual "rec {inherit a; }" ["a"]
  freeVarsEqual "let inherit a; in { }" ["a"]

case_empty_string_equal_null_is_false =
  constantEqualText "false" "\"\" == null"

case_null_equal_empty_string_is_false =
  constantEqualText "false" "null == \"\""

case_empty_string_not_equal_null_is_true =
  constantEqualText "true" "\"\" != null"

case_null_equal_not_empty_string_is_true =
  constantEqualText "true" "null != \"\""

case_list_nested_bottom_diverges =
  assertNixEvalThrows "let nested = [(let x = x; in x)]; in nested == nested"

case_attrset_nested_bottom_diverges =
  assertNixEvalThrows "let nested = { y = (let x = x; in x); }; in nested == nested"

case_list_list_nested_bottom_equal =
  constantEqualText "true" "let nested = [[(let x = x; in x)]]; in nested == nested"

case_list_attrset_nested_bottom_equal =
  constantEqualText "true" "let nested = [{ y = (let x = x; in x); }]; in nested == nested"

case_list_function_nested_bottom_equal =
  constantEqualText "true" "let nested = [(_: let x = x; in x)]; in nested == nested"

case_attrset_list_nested_bottom_equal =
  constantEqualText "true" "let nested = { y = [(let x = x; in x)];}; in nested == nested"

case_attrset_attrset_nested_bottom_equal =
  constantEqualText "true" "let nested = { y = { y = (let x = x; in x); }; }; in nested == nested"

case_attrset_function_nested_bottom_equal =
  constantEqualText "true" "let nested = { y = _: (let x = x; in x); }; in nested == nested"

-----------------------

tests :: TestTree
tests = $testGroupGenerator

genEvalCompareTests = do
    td <- D.listDirectory testDir

    let unmaskedFiles = filter ((==".nix") . takeExtension) td
    let files = unmaskedFiles \\ maskedFiles

    return $ testGroup "Eval comparison tests" $ map (mkTestCase testDir) files
  where
    mkTestCase td f = testCase f $ assertEvalFileMatchesNix (td </> f)

constantEqual :: NExprLoc -> NExprLoc -> Assertion
constantEqual a b = do
    time <- getCurrentTime
    let opts = defaultOptions time
    -- putStrLn =<< lint (stripAnnotation a)
    res <- runWithBasicEffectsIO opts $ do
        a' <- normalForm =<< nixEvalExprLoc Nothing a
        b' <- normalForm =<< nixEvalExprLoc Nothing b
        valueEqM a' b'
    assertBool "" res

constantEqualText' :: Text -> Text -> Assertion
constantEqualText' a b = do
  let Success a' = parseNixTextLoc a
      Success b' = parseNixTextLoc b
  constantEqual a' b'

constantEqualText :: Text -> Text -> Assertion
constantEqualText a b = do
  constantEqualText' a b
  mres <- liftIO $ lookupEnv "ALL_TESTS" <|> lookupEnv "MATCHING_TESTS"
  when (isJust mres) $
      assertEvalMatchesNix b

assertNixEvalThrows :: Text -> Assertion
assertNixEvalThrows a = do
  let Success a' = parseNixTextLoc a
  time <- getCurrentTime
  let opts = defaultOptions time
  errored <- catch
      (False <$ runWithBasicEffectsIO opts
         (normalForm =<< nixEvalExprLoc Nothing a'))
      (\(_ :: NixException) -> pure True)
  unless errored $
    assertFailure "Did not catch nix exception"

freeVarsEqual :: Text -> [VarName] -> Assertion
freeVarsEqual a xs = do
  let Success a' = parseNixText a
      xs' = S.fromList xs
      free = freeVars a'
  assertEqual "" xs' free

maskedFiles :: [FilePath]
maskedFiles =
  [ "builtins.fetchurl-01.nix" ]

testDir :: FilePath
testDir = "tests/eval-compare"
