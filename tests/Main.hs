module Main where

import Test.Tasty

import qualified ParserTests
import qualified EvalTests
import qualified ShorthandTests
import qualified PrettyTests
import qualified NixLanguageTests

import Prelude (IO, ($))

main :: IO ()
main = do
  nixLanguageTests <- NixLanguageTests.genTests
  defaultMain $ testGroup "hnix"
    [ ParserTests.tests
    , EvalTests.tests
    , ShorthandTests.tests
    , PrettyTests.tests
    , nixLanguageTests
    ]
