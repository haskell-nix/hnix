# hnix

Haskell parser, evaluator and type checker for the Nix language.

## Prerequisites

Nix is installed and in your `$PATH`.

## Getting Started

```bash
$ git clone https://github.com/jwiegley/hnix.git
...
$ cd hnix
$ cabal2nix --shell . > default.nix
$ nix-shell
...
$ cabal test
...
$ cabal bench
...
$ ./dist/build/hnix/hnix --help
```

