# hnix

| [![Chat](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/haskell-nix/Lobby)                                                                                  | CI                                                                                                                                                                                                                                         |
| :---                                                                                                                                                                     |  :---                                                                                                                                                                                                                                      |
| [![Hackage](https://img.shields.io/hackage/v/hnix?color=%235e5086&label=Latest%20release)](https://hackage.haskell.org/package/hnix)                                      | [![Hackage, Cabal, Linux](https://github.com/haskell-nix/hnix/workflows/Hackage,%20Cabal,%20Linux/badge.svg)](https://github.com/haskell-nix/hnix/actions?query=workflow%3A"Hackage%2C+Cabal%2C+Linux"+branch%3Amaster)                    |
| [![Hackage Matrix Builder](https://img.shields.io/badge/Hackage%20Matrix-Builder-%235e5086)](https://matrix.hackage.haskell.org/package/hnix)                             | [![Nixpkgs, Linux, main](https://github.com/haskell-nix/hnix/workflows/Nixpkgs,%20Linux,%20main/badge.svg)](https://github.com/haskell-nix/hnix/actions?query=workflow%3A%22Nixpkgs%2C+Linux%2C+main%22+branch%3Amaster)                   |
| [![Nixpkgs Hydra CI](https://img.shields.io/badge/Nixpkgs%20Hydra-CI-%234f72bb)](https://hydra.nixos.org/job/nixpkgs/trunk/haskellPackages.hnix.x86_64-linux#tabs-status) | [![Nixpkgs, Linux, additional](https://github.com/haskell-nix/hnix/workflows/Nixpkgs,%20Linux,%20additional/badge.svg)](https://github.com/haskell-nix/hnix/actions?query=workflow%3A%22Nixpkgs%2C+Linux%2C+additional%22+branch%3Amaster) |
| [![Dependencies](https://img.shields.io/hackage-deps/v/hnix?label=Dependencies)](https://packdeps.haskellers.com/feed?needle=hnix)                                        | [![Nixpkgs, macOS](https://github.com/haskell-nix/hnix/workflows/Nixpkgs,%20macOS/badge.svg)](https://github.com/haskell-nix/hnix/actions?query=workflow%3A%22Nixpkgs%2C+macOS%22+branch%3Amaster)                                         |
| [![Repology page](https://img.shields.io/badge/Repology-page-%23005500)](https://repology.org/project/haskell:hnix/versions)                                              | |

Haskell parser, evaluator and type checker for the Nix language.

## Prerequisites

Nix is installed and in your `$PATH`. This is so that `nix-store` can be used
for interacting with store paths, until `hnix-store` is ready.

## Getting Started

```
$ git clone --recursive https://github.com/haskell-nix/hnix.git
...
$ cd hnix
$ nix-shell
$ cabal v2-configure --enable-tests
$ cabal v2-build
$ cabal v2-test
# To run all of the tests, which takes up to a minute:
$ env ALL_TESTS=yes cabal v2-test
# To run only specific tests (see `tests/Main.hs` for a list)
$ env NIXPKGS_TESTS=yes PRETTY_TESTS=1 cabal v2-test
$ ./dist/build/hnix/hnix --help
```

## Using the REPL

To enter the `hnix` REPL use

```
hnix --repl
```

To evaluate an expression and make it available in the REPL
as the `input` variable use

```
hnix --eval -E '(import <nixpkgs> {}).pkgs.hello' --repl
```

Use the `:help` command for a list of all available REPL commands.

## Building with full debug info

To build `hnix` for debugging, and with full tracing output and stack traces,
use:

```
$ nix-shell
$ cabal v2-configure --enable-tests --enable-profiling --flags=profiling --flags=tracing
$ cabal v2-build
$ ./dist/build/hnix/hnix -v5 --trace <args> +RTS -xc
```

Note that this will run quite slowly, but will give the most information as to
what might potentially be going wrong during parsing or evaluation.

## Building with benchmarks enabled

To build `hnix` with benchmarks enabled:

```
$ nix-shell --arg doBenchmarks true
$ cabal v2-configure --enable-tests --enable-benchmarks
$ cabal v2-build
$ cabal v2-bench
```

## Building with profiling enabled

To build `hnix` with profiling enabled:

```
$ nix-shell
$ cabal v2-configure --enable-tests --enable-profiling --flags=profiling
$ cabal v2-build
$ ./dist/build/hnix/hnix <args> +RTS -p
```

## Building with GHCJS

From the project root directory, run:

```
$ NIX_CONF_DIR=$PWD/ghcjs nix-build ghcjs
```

This will build an `hnix` library that can be linked to your GHCJS
application.

## Using the Cachix binary cache

If you're on macOS, you can use the binary cache at Cachix to avoid building
the specific dependencies used by hnix. Just use these commands:

```
$ nix-env -iA cachix -f https://github.com/NixOS/nixpkgs/tarball/db557aab7b690f5e0e3348459f2e4dc8fd0d9298
$ cachix use hnix
```

## How you can help

### Issue Tracker Backlog

If you're looking for a way to help out, try taking a look
[here](https://github.com/haskell-nix/hnix/issues?q=is%3Aissue+is%3Aopen+label%3A%22help+wanted%22+no%3Aassignee).
When you find an issue that looks interesting to you, comment on the ticket to
let others know you're working on it; look for others who might have done the
same. You can talk with everyone live on
[Gitter](https://gitter.im/haskell-nix/Lobby).

When you're ready to submit a pull request, test it with:

```
$ git submodule update --init --recursive
$ nix-shell --run "LANGUAGE_TESTS=yes cabal v2-test"
```

Make sure that all the tests that were passing prior to your PR are still
passing afterwards; it's OK if no new tests are passing.

### Evaluating Nixpkgs with HNix

Currently the main high-level goal is to be able to evaluate all of nixpkgs. To
run this yourself, first build hnix with `nix-build`, then run the following
command:

```
$ ./result/bin/hnix --eval -E "import <nixpkgs> {}" --find
```
