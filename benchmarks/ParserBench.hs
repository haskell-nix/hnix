module ParserBench (benchmarks) where

import           Nix.Parser

import           Criterion

benchFile :: FilePath -> Benchmark
benchFile = bench <*> whnfIO . parseNixFile . ("data/" <>)

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
