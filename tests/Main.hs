module Main where

import           Control.Monad
import           Data.List (isInfixOf)
import qualified EvalTests
import qualified NixLanguageTests
import qualified ParserTests
import qualified PrettyTests
import           System.Environment
import           System.Exit
import           System.Posix.Files
import           System.Process
import           Test.Tasty
import           Test.Tasty.HUnit

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
                  ++ " \"git submodule update --init --recursive\"?"
            ]

ensureNixpkgsCanParse :: Assertion
ensureNixpkgsCanParse = do
  -- jww (2018-04-10): Use hnix to parse default.nix and pull this data from there!
  path <- readCreateProcess (shell "nix-instantiate --eval --expr '\"${let hostPkgs = import <nixpkgs> {}; in hostPkgs.fetchFromGitHub { owner = \"NixOS\"; repo = \"nixpkgs-channels\"; rev = \"ee28e35ba37ab285fc29e4a09f26235ffe4123e2\"; sha256 = \"0a6xrqjj2ihkz1bizhy5r843n38xgimzw5s2mfc42kk2rgc95gw5\"; }}\"'") ""
  (exit, _, errs) <- readCreateProcessWithExitCode (shell $ "find " ++ init path ++ " -name '*.nix' | ./dist/build/hnix/hnix --ignore-errors --parse-only -f - > /dev/null") ""
  unless (exit == ExitSuccess) $ errorWithoutStackTrace errs

main :: IO ()
main = do
  nixLanguageTests <- NixLanguageTests.genTests
  langTestsEnv <- lookupEnv "LANGUAGE_TESTS"
  nixpkgsTestsEnv <- lookupEnv "NIXPKGS_TESTS"
  let runLangTests = langTestsEnv == Just "yes"
  let runNixpkgsTests = nixpkgsTestsEnv == Just "yes"
  defaultMain $ testGroup "hnix" $
    [ testCase "hnix.cabal correctly generated" cabalCorrectlyGenerated ] ++
    [ ParserTests.tests
    , EvalTests.tests
    , PrettyTests.tests
    ] ++
    [ testCase "Nix languarge tests present" ensureLangTestsPresent | runLangTests ] ++
    [ nixLanguageTests | runLangTests ] ++
    [ testCase "Nixpkgs parses without errors" ensureNixpkgsCanParse | runNixpkgsTests ]
