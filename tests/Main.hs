module Main where

import Test.Tasty

import qualified ParserTests

main :: IO ()
main = defaultMain $ testGroup "hnix"
  [ ParserTests.tests
  ]
