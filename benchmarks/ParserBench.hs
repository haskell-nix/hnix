module ParserBench (benchmarks) where

import           Nix.Parser

import           Control.Applicative
import           Criterion

benchFile :: FilePath -> Benchmark
benchFile = bench <*> whnfIO . parseNixFile . ("data/" ++)

benchmarks :: Benchmark
benchmarks = bgroup
  "Parser"
  [ benchFile "nixpkgs-all-packages.nix"
  , benchFile "nixpkgs-all-packages-pretty.nix"
  , benchFile "let-comments.nix"
  , benchFile "let-comments-multiline.nix"
  , benchFile "let.nix"
  , benchFile "simple.nix"
  , benchFile "simple-pretty.nix"
  ]
