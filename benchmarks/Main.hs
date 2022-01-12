module Main where

import           Nix.Prelude
import           Criterion.Main

import qualified ParserBench

main :: IO ()
main = defaultMain $ one ParserBench.benchmarks
