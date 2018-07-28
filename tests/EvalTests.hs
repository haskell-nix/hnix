{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-missing-signatures -Wno-orphans #-}

module EvalTests (tests, genEvalCompareTests) where

import           Control.Monad.Catch
import           Control.Monad (when)
import           Control.Monad.IO.Class
import qualified Data.HashMap.Lazy as M
import           Data.Maybe (isJust)
import           Data.String.Interpolate.IsString
import           Data.Text (Text)
import           Data.Time
import           Nix
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

-- jww (2018-05-02): This constantly changes!
-- case_placeholder =
--   constantEqualText
--       "\"/1rz4g4znpzjwh1xymhjpm42vipw92pr73vdgl6xs1hycac8kf2n9\""
--       "builtins.placeholder \"out\""

-----------------------

tests :: TestTree
tests = $testGroupGenerator

genEvalCompareTests = do
    files <- filter ((==".nix") . takeExtension) <$> D.listDirectory testDir
    return $ testGroup "Eval comparison tests" $ map mkTestCase files
  where
    testDir = "tests/eval-compare"
    mkTestCase f = testCase f $ assertEvalFileMatchesNix (testDir </> f)

instance (Show r, Show (NValueF m r), Eq r) => Eq (NValueF m r) where
    NVConstantF x == NVConstantF y = x == y
    NVStrF ls == NVStrF rs = stringIntentionallyDropContext ls == stringIntentionallyDropContext rs
    NVListF x == NVListF y = and (zipWith (==) x y)
    NVSetF x _ == NVSetF y _ =
        M.keys x == M.keys y &&
        and (zipWith (==) (M.elems x) (M.elems y))
    NVPathF x == NVPathF y = x == y
    x == y = error $ "Need to add comparison for values: "
                 ++ show x ++ " == " ++ show y

constantEqual :: NExprLoc -> NExprLoc -> Assertion
constantEqual a b = do
    time <- liftIO getCurrentTime
    let opts = defaultOptions time
    -- putStrLn =<< lint (stripAnnotation a)
    a' <- runLazyM opts $ normalForm =<< nixEvalExprLoc Nothing a
    -- putStrLn =<< lint (stripAnnotation b)
    b' <- runLazyM opts $ normalForm =<< nixEvalExprLoc Nothing b
    assertEqual "" a' b'

constantEqualText' :: Text -> Text -> Assertion
constantEqualText' a b = do
  let Success a' = parseNixTextLoc a
      Success b' = parseNixTextLoc b
  constantEqual a' b'

constantEqualText :: Text -> Text -> Assertion
constantEqualText a b = do
  constantEqualText' a b
  mres <- liftIO $ lookupEnv "MATCHING_TESTS"
  when (isJust mres) $
      assertEvalMatchesNix b

assertNixEvalThrows :: Text -> Assertion
assertNixEvalThrows a = do
    let Success a' = parseNixTextLoc a
    time <- liftIO getCurrentTime
    let opts = defaultOptions time
    errored <- catch ((runLazyM opts $ normalForm =<< nixEvalExprLoc Nothing a') >> pure False) handler
    if errored then
        pure ()
    else
        assertFailure "Did not catch nix exception"
    where
       handler :: NixException -> IO Bool
       handler _ = pure True
