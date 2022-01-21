{-# language QuasiQuotes #-}

module Main where

import           Nix.Prelude
import           Relude (force)
import           Relude.Unsafe (read)
import qualified Control.Exception as Exc
import           GHC.Err (errorWithoutStackTrace)
import           Data.Fix
import           Data.List (isSuffixOf, lookup)
import qualified Data.String as String
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
import           System.Environment (setEnv)
import           System.FilePath.Glob (compile, globDir1)
import           System.PosixCompat.Files
import           Test.Tasty
import           Test.Tasty.HUnit

ensureLangTestsPresent :: Assertion
ensureLangTestsPresent = do
  exist <- fileExist "data/nix/tests/local.mk"
  unless exist $
    errorWithoutStackTrace $ String.unlines
      [ "Directory data/nix does not have any files."
      , "Did you forget to run"
          <> " \"git submodule update --init --recursive\"?" ]

ensureNixpkgsCanParse :: Assertion
ensureNixpkgsCanParse =
  consider "default.nix" (parseNixFile "default.nix") $ \case
    Fix (NAbs (ParamSet _ _ pset) _) -> do
      let rev    = getString "rev" pset
          sha256 = getString "sha256" pset
      consider "fetchTarball expression" (pure $ parseNixTextLoc [text|
        builtins.fetchTarball {
          url    = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
          sha256 = "${sha256}";
        }|]) $ \expr -> do
        NVStr ns <- do
          time <- getCurrentTime
          runWithBasicEffectsIO (defaultOptions time) $
            Nix.nixEvalExprLoc mempty expr
        let dir = getStringIgnoreContext ns
        exists <- fileExist $ toString dir
        unless exists $
          errorWithoutStackTrace $
            "Directory " <> show dir <> " does not exist"
        files <- globDir1 (compile "**/*.nix") $ toString dir
        when (null files) $
          errorWithoutStackTrace $
            "Directory " <> show dir <> " does not have any files"
        for_ files $ \file ->
          unless ("azure-cli/default.nix" `isSuffixOf` file ||
                  "os-specific/linux/udisks/2-default.nix"  `isSuffixOf` file) $ do
            -- Parse and deepseq the resulting expression tree, to ensure the
            -- parser is fully executed.
            _ <- consider (coerce file) (parseNixFileLoc (coerce file)) $ Exc.evaluate . force
            stub
    v -> fail $ "Unexpected parse from default.nix: " <> show v
 where
  getExpr   k m =
    let Just (Just r) = lookup k m in
    r
  getString k m =
    let Fix (NStr (DoubleQuoted [Plain str])) = getExpr k m in
    str

  consider path action k =
    either
      (\ err -> errorWithoutStackTrace $ "Parsing " <> coerce @Path path <> " failed: " <> show err)
      k
      =<< action

main :: IO ()
main = do
  nixLanguageTests    <- NixLanguageTests.genTests
  evalComparisonTests <- EvalTests.genEvalCompareTests
  let allOrLookup var = lookupEnv "ALL_TESTS" <|> lookupEnv var
  nixpkgsTestsEnv     <- allOrLookup "NIXPKGS_TESTS"
  prettyTestsEnv      <- lookupEnv "PRETTY_TESTS"

  pwd <- getCurrentDirectory
  setEnv "NIX_REMOTE" $ pwd <> "/real-store"
  setEnv "NIX_DATA_DIR" $ pwd <> "/data"

  defaultMain $ testGroup "hnix" $
    [ ParserTests.tests
    , EvalTests.tests
    , PrettyTests.tests
    , ReduceExprTests.tests
    , PrettyParseTests.tests $ fromIntegral $ read @Int $ fromMaybe "0" prettyTestsEnv
    , evalComparisonTests
    , testCase "Nix language tests present" ensureLangTestsPresent
    , nixLanguageTests ] <>
    [ testCase "Nixpkgs parses without errors" ensureNixpkgsCanParse
      | isJust nixpkgsTestsEnv
    ]
