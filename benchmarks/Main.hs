{-# LANGUAGE CPP #-}
module Main where

#if !defined(ghcjs_HOST_OS)
import Criterion.Main
#endif

import qualified ParserBench

main :: IO ()
main = do
#if defined(ghcjs_HOST_OS)
    putStrLn "Main cannot be built with GHCJS"
#else
  defaultMain
  [ ParserBench.benchmarks
  ]
#endif
