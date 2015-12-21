module Main where

import Test.Tasty

import qualified ParserTests
import qualified EvalTests

main :: IO ()
main = defaultMain $ testGroup "hnix"
  [ ParserTests.tests
  , EvalTests.tests
  ]
