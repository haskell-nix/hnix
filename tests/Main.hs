module Main where

import           Control.Monad
import           Data.List (isInfixOf)
import qualified EvalTests
import qualified NixLanguageTests
import qualified ParserTests
import qualified PrettyTests
import           System.Environment
import           System.Posix.Files
import           System.Process
import           Test.Tasty

main :: IO ()
main = do
  nixLanguageTests <- NixLanguageTests.genTests

  langTestsEnv <- lookupEnv "LANGUAGE_TESTS"
  let runLangTests = langTestsEnv == Just "yes"
  when runLangTests $ do
    exist <- fileExist "data/nix/tests/local.mk"
    unless exist $
        errorWithoutStackTrace $ unlines
            [ "Directory data/nix does not have any files."
            , "Did you forget to run"
                  ++ " \"git submodule update --init --recursive\"?"
            ]

  output <- readCreateProcess (shell "hpack") ""
  when ("modified manually" `isInfixOf` output) $
    errorWithoutStackTrace
      "Edit package.yaml and re-generate hnix.cabal by running \"hpack\""

  defaultMain $ testGroup "hnix" $
    [ ParserTests.tests
    , EvalTests.tests
    , PrettyTests.tests
    ] ++
    [ nixLanguageTests | runLangTests ]
