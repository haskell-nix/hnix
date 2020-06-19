{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.DeepSeq
import qualified Control.Exception as Exc
import           Control.Applicative ((<|>))
import           Control.Monad
import           Data.Fix
import           Data.List (isSuffixOf)
import           Data.Maybe
import           Data.Text (unpack)
import           Data.Time
import qualified EvalTests
import           NeatInterpolation (text)
import qualified Nix
import           Nix.Expr.Types
import           Nix.String
import           Nix.Options
import           Nix.Parser
import           Nix.Standard
import           Nix.Value
import qualified NixLanguageTests
import qualified ParserTests
import qualified PrettyTests
import qualified ReduceExprTests
import qualified PrettyParseTests
import           System.Directory
import           System.Environment
import           System.FilePath.Glob
import           System.Posix.Files
import           Test.Tasty
import           Test.Tasty.HUnit

ensureLangTestsPresent :: Assertion
ensureLangTestsPresent = do
  exist <- fileExist "data/nix/tests/local.mk"
  unless exist $
    errorWithoutStackTrace $ unlines
      [ "Directory data/nix does not have any files."
      , "Did you forget to run"
          ++ " \"git submodule update --init --recursive\"?" ]

ensureNixpkgsCanParse :: Assertion
ensureNixpkgsCanParse =
  consider "default.nix" (parseNixFile "default.nix") $ \case
    Fix (NAbs (ParamSet params _ _) _) -> do
      let rev    = getString "rev" params
          sha256 = getString "sha256" params
      consider "fetchTarball expression" (pure $ parseNixTextLoc [text|
        builtins.fetchTarball {
          url    = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
          sha256 = "${sha256}";
        }|]) $ \expr -> do
        NVStr ns <- do
          time <- getCurrentTime
          runWithBasicEffectsIO (defaultOptions time) $
            Nix.nixEvalExprLoc Nothing expr
        let dir = hackyStringIgnoreContext ns
        exists <- fileExist (unpack dir)
        unless exists $
          errorWithoutStackTrace $
            "Directory " ++ show dir ++ " does not exist"
        files <- globDir1 (compile "**/*.nix") (unpack dir)
        when (null files) $
          errorWithoutStackTrace $
            "Directory " ++ show dir ++ " does not have any files"
        forM_ files $ \file -> do
          unless ("azure-cli/default.nix" `isSuffixOf` file ||
                  "os-specific/linux/udisks/2-default.nix"  `isSuffixOf` file) $ do
            -- Parse and deepseq the resulting expression tree, to ensure the
            -- parser is fully executed.
            _ <- consider file (parseNixFileLoc file) $ Exc.evaluate . force
            return ()
    v -> error $ "Unexpected parse from default.nix: " ++ show v
 where
  getExpr   k m = let Just (Just r) = lookup k m in r
  getString k m =
      let Fix (NStr (DoubleQuoted [Plain str])) = getExpr k m in str

  consider path action k = action >>= \case
    Failure err -> errorWithoutStackTrace $
      "Parsing " ++ path ++ " failed: " ++ show err
    Success expr -> k expr

main :: IO ()
main = do
  nixLanguageTests    <- NixLanguageTests.genTests
  evalComparisonTests <- EvalTests.genEvalCompareTests
  let allOrLookup var = lookupEnv "ALL_TESTS" <|> lookupEnv var
  nixpkgsTestsEnv     <- allOrLookup "NIXPKGS_TESTS"
  prettyTestsEnv      <- lookupEnv "PRETTY_TESTS"

  pwd <- getCurrentDirectory
  setEnv "NIX_REMOTE" ("local?root=" ++ pwd ++ "/")
  setEnv "NIX_DATA_DIR" (pwd ++ "/data")

  defaultMain $ testGroup "hnix" $
    [ ParserTests.tests
    , EvalTests.tests
    , PrettyTests.tests
    , ReduceExprTests.tests] ++
    [ PrettyParseTests.tests
        (fromIntegral (read (fromMaybe "0" prettyTestsEnv) :: Int)) ] ++
    [ evalComparisonTests ] ++
    [ testCase "Nix language tests present" ensureLangTestsPresent
    , nixLanguageTests ] ++
    [ testCase "Nixpkgs parses without errors" ensureNixpkgsCanParse
      | isJust nixpkgsTestsEnv ]
