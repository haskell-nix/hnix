module Main where

import           Criterion.Main

import qualified ParserBench

main :: IO ()
main = defaultMain [ParserBench.benchmarks]
