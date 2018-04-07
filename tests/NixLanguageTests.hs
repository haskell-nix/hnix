{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module NixLanguageTests (genTests) where

import           Control.Arrow ((&&&))
import           Control.Exception
import           Control.Monad
import           Control.Monad.ST
import           Data.List (delete, sort)
import           Data.List.Split (splitOn)
import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import           GHC.Exts
import           Nix
import           Nix.Monad.Instance
import           Nix.Parser
import           Nix.Pretty
import           Nix.Utils
import           Nix.Stack
import           Nix.Value
import           Nix.XML
import           System.Environment
import           System.FilePath
import           System.FilePath.Glob (compile, globDir1)
import           System.IO
import           System.Posix.Temp
import           System.Process
import           Test.Tasty
import           Test.Tasty.HUnit
import           Text.Printf
import           TestCommon

{-
From (git://nix)/tests/lang.sh we see that

    lang/parse-fail-*.nix -> parsing should fail
    lang/parse-okay-*.nix -> parsing should succeed
    lang/eval-fail-*.nix -> eval should fail

    lang/eval-okay-*.{nix,xml} -> eval should succeed,
        xml dump should be the same as the .xml
    lang/eval-okay-*.{nix,exp} -> eval should succeed,
        plain text output should be the same as the .exp
    lang/eval-okay-*.{nix,exp,flags} -> eval should succeed,
        plain text output should be the same as the .exp,
        pass the extra flags to nix-instantiate

    NIX_PATH=lang/dir3:lang/dir4 should be in the environment of all
        eval-okay-*.nix evaluations
    TEST_VAR=foo should be in all the environments # for eval-okay-getenv.nix
-}

groupBy :: Ord k => (v -> k) -> [v] -> Map k [v]
groupBy key = Map.fromListWith (++) . map (key &&& pure)

genTests :: IO TestTree
genTests = do
  testFiles <- sort . filter ((/= ".xml") . takeExtension)
      <$> globDir1 (compile "*-*-*.*") "data/nix/tests/lang"
  let testsByName = groupBy (takeFileName . dropExtensions) testFiles
  let testsByType = groupBy testType (Map.toList testsByName)
  let testGroups  = map mkTestGroup (Map.toList testsByType)
  return $ localOption (mkTimeout 100000)
         $ testGroup "Nix (upstream) language tests" testGroups
  where
    testType (fullpath, _files) =
        take 2 $ splitOn "-" $ takeFileName fullpath
    mkTestGroup (kind, tests) =
        testGroup (unwords kind) $ map (mkTestCase kind) tests
    mkTestCase kind (basename, files) =
        testCase (takeFileName basename) $ case kind of
            ["parse", "okay"] -> assertParse $ the files
            ["parse", "fail"] -> assertParseFail $ the files
            ["eval", "okay"] -> assertEval files
            ["eval", "fail"] -> assertEvalFail $ the files
            _ -> error $ "Unexpected: " ++ show kind

assertParse :: FilePath -> Assertion
assertParse file = parseNixFile file >>= \case
  Success expr -> pure $! runST $ void $ lint expr
  Failure err  ->
      assertFailure $ "Failed to parse " ++ file ++ ":\n" ++ show err

assertParseFail :: FilePath -> Assertion
assertParseFail file = do
    eres <- parseNixFile file
    catch (case eres of
               Success expr -> do
                   _ <- pure $! runST $ void $ lint expr
                   assertFailure $ "Unexpected success parsing `"
                       ++ file ++ ":\nParsed value: " ++ show expr
               Failure _ -> return ()) $ \(_ :: SomeException) ->
        return ()

assertLangOk :: FilePath -> Assertion
assertLangOk file = do
  actual <- printNix <$> hnixEvalFile (file ++ ".nix")
  expected <- Text.readFile $ file ++ ".exp"
  assertEqual "" expected $ Text.pack (actual ++ "\n")

assertLangOkXml :: FilePath -> Assertion
assertLangOkXml file = do
  actual <- toXML <$> hnixEvalFile (file ++ ".nix")
  expected <- Text.readFile $ file ++ ".exp.xml"
  assertEqual "" expected $ Text.pack actual

assertEval :: [FilePath] -> Assertion
assertEval files = catch go $ \case
    NixEvalException str -> error $ "Evaluation error: " ++ str
  where
    go = case delete ".nix" $ sort $ map takeExtensions files of
        [] -> assertLangOkXml name
        [".exp"] -> assertLangOk name
        [".exp.disabled"] -> return ()
        [".exp-disabled"] -> return ()
        [".exp", ".flags"] ->
            assertFailure $ "Support for flags not implemented (needed by "
                ++ name ++ ".nix)."
        _ -> assertFailure $ "Unknown test type " ++ show files
      where
        name = "data/nix/tests/lang/"
            ++ the (map (takeFileName . dropExtensions) files)

assertEvalFail :: FilePath -> Assertion
assertEvalFail file = catch ?? (\(_ :: SomeException) -> return ()) $ do
  evalResult <- printNix <$> hnixEvalFile file
  evalResult `seq` assertFailure $
      file ++ " should not evaluate.\nThe evaluation result was `"
           ++ evalResult ++ "`."
