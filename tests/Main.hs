module Main where

import Test.Tasty

import qualified ParserTests
import qualified EvalTests

import Prelude (IO, ($))

main :: IO ()
main = defaultMain $ testGroup "hnix"
  [ ParserTests.tests
  , EvalTests.tests
  ]
