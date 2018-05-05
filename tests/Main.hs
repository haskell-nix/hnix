{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

#if !defined(ghcjs_HOST_OS)
import           Control.DeepSeq
import qualified Control.Exception as Exc
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Fix
import           Data.List (isInfixOf)
import           Data.Maybe (isJust)
import           Data.String.Interpolate.IsString
import           Data.Text (unpack)
import           Data.Time
import qualified EvalTests
import qualified Nix
import           Nix.Exec
import           Nix.Expr.Types
import           Nix.Options
import           Nix.Parser
import           Nix.Value
import qualified NixLanguageTests
import qualified ParserTests
import qualified PrettyTests
import           System.Environment
import           System.FilePath.Glob
import           System.Posix.Files
import           System.Process
import           Test.Tasty
import           Test.Tasty.HUnit
#endif

main :: IO ()
main = do
#if defined(ghcjs_HOST_OS)
    putStrLn "Main cannot be built with GHCJS"
#else
  nixLanguageTests    <- NixLanguageTests.genTests
  evalComparisonTests <- EvalTests.genEvalCompareTests
  langTestsEnv        <- lookupEnv "LANGUAGE_TESTS"
  nixpkgsTestsEnv     <- lookupEnv "NIXPKGS_TESTS"
  let runLangTests    = isJust langTestsEnv
  let runNixpkgsTests = isJust nixpkgsTestsEnv

  defaultMain $ testGroup "hnix" $
    [ testCase "hnix.cabal correctly generated" cabalCorrectlyGenerated ] ++
    [ ParserTests.tests
    , EvalTests.tests
    , PrettyTests.tests
    , evalComparisonTests ] ++
    [ testCase "Nix language tests present" ensureLangTestsPresent
    | runLangTests ] ++
    [ nixLanguageTests | runLangTests ] ++
    [ testCase "Nixpkgs parses without errors" ensureNixpkgsCanParse
    | runNixpkgsTests ]

cabalCorrectlyGenerated :: Assertion
cabalCorrectlyGenerated = do
  output <- readCreateProcess (shell "hpack") ""
  when ("modified manually" `isInfixOf` output) $
    errorWithoutStackTrace
      "Edit package.yaml and re-generate hnix.cabal by running \"hpack\""

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
      consider "fetchTarball expression" (pure $ parseNixTextLoc [i|
        builtins.fetchTarball {
          url    = "https://github.com/NixOS/nixpkgs/archive/#{rev}.tar.gz";
          sha256 = "#{sha256}";
        }|]) $ \expr -> do
        NVStr dir _ <- do
            time <- liftIO getCurrentTime
            runLazyM (defaultOptions time) $ Nix.nixEvalExprLoc Nothing expr
        files <- globDir1 (compile "**/*.nix") (unpack dir)
        forM_ files $ \file ->
          -- Parse and deepseq the resulting expression tree, to ensure the
          -- parser is fully executed.
          consider file (parseNixFileLoc file) $ Exc.evaluate . force
    v -> error $ "Unexpected parse from default.nix: " ++ show v
 where
  getExpr   k m = let Just (Just r) = lookup k m in r
  getString k m =
      let Fix (NStr (DoubleQuoted [Plain str])) = getExpr k m in str

  consider path action k = action >>= \case
    Failure err -> errorWithoutStackTrace $
      "Parsing " ++ path ++ " failed: " ++ show err
    Success expr -> k expr
#endif
