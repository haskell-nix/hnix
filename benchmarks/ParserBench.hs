module ParserBench (benchmarks) where

import           Nix.Utils
import           Nix.Parser

import           Criterion

benchFile :: Path -> Benchmark
benchFile = bench . coerce <*> whnfIO . parseNixFile . ("data/" <>)

benchmarks :: Benchmark
benchmarks = bgroup
  "Parser" $
    fmap benchFile
      [ "nixpkgs-all-packages.nix"
      , "nixpkgs-all-packages-pretty.nix"
      , "let-comments.nix"
      , "let-comments-multiline.nix"
      , "let.nix"
      , "simple.nix"
      , "simple-pretty.nix"
      ]
