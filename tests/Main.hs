module Main where

import           Control.Monad
import qualified EvalTests
import qualified NixLanguageTests
import qualified ParserTests
import qualified PrettyTests
import           System.Environment
import           System.Posix.Files
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
            , "Did you forget to run \"git submodule update --init --recursive\"?"
            ]
  defaultMain $ testGroup "hnix" $
    [ ParserTests.tests
    , EvalTests.tests
    , PrettyTests.tests
    ] ++
    [ nixLanguageTests | runLangTests ]
