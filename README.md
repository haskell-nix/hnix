# hnix

Haskell parser for Nix expression language.

## Prerequisites

Nix is installed and in your `$PATH`.
Tested with Nix v1.10.

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
```

