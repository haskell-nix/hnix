module Main where

import Criterion.Main
import Nix.Prelude

import qualified ParserBench

main :: IO ()
main = defaultMain $ one ParserBench.benchmarks
