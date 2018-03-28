module Main where

import qualified EvalTests
import qualified NixLanguageTests
import qualified ParserTests
import qualified PrettyTests
import           Test.Tasty

main :: IO ()
main = do
  nixLanguageTests <- NixLanguageTests.genTests
  defaultMain $ testGroup "hnix"
    [ ParserTests.tests
    , EvalTests.tests
    , PrettyTests.tests
    , nixLanguageTests
    ]
