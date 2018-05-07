# hnix

[![Build Status](https://api.travis-ci.org/haskell-nix/hnix.svg)](https://travis-ci.org/haskell-nix/hnix)
[![Chat](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/haskell-nix/Lobby)

Haskell parser, evaluator and type checker for the Nix language.

## Prerequisites

Nix is installed and in your `$PATH`. This is so that `nix-store` can be used
for interacting with store paths, until `hnix-store` is ready.

## Getting Started

```bash
$ git clone --recursive https://github.com/haskell-nix/hnix.git
...
$ cd hnix
$ nix-shell
$ cabal configure --enable-tests
$ cabal build
$ cabal test
# To run all of the tests, which takes up to a minute:
$ LANGUAGE_TESTS=yes NIXPKGS_TESTS=yes cabal test
$ ./dist/build/hnix/hnix --help
```

## Building a Docker container

If you don't have Nix installed, or you'd just like to play around with `hnix`
completely separately from your main system, you can build a Docker container:

```bash
$ docker build -t hnix .
$ docker run hnix hnix --eval --expr '1 + 2'

# In order to refer to files under the current directory:
$ docker run -v $PWD/:/tmp/build run hnix hnix default.nix
```

## Building with full debug info

To build `hnix` for debugging, and with full tracing output and stack traces,
use:

```
$ nix-shell --arg doProfiling true
$ cabal configure --enable-tests --enable-profiling --flags=tracing
$ cabal build
$ ./dist/build/hnix/hnix -v5 --trace <args> +RTS -xc
```

Note that this will run quite slowly, but will give the most information as to
what might potentially be going wrong during parsing or evaluation.

## Building with benchmarks enabled

To build `hnix` with benchmarks enabled:

```
$ nix-shell --arg doBenchmarks true
$ cabal configure --enable-tests --enable-benchmarks
$ cabal build
$ cabal bench
```

## Building with profiling enabled

To build `hnix` with profiling enabled:

```
$ nix-shell --arg doProfiling true
$ cabal configure --enable-tests --enable-profiling
$ cabal build
$ ./dist/build/hnix/hnix <args> +RTS -p
```

## How you can help

If you're looking for a way to help out, try taking a look
[here](https://github.com/haskell-nix/hnix/issues?q=is%3Aissue+is%3Aopen+label%3A%22help+wanted%22+no%3Aassignee).
When you find an issue that looks interesting to you, comment on the ticket to
let others know you're working on it; look for others who might have done the
same. You can talk with everyone live on
[Gitter](https://gitter.im/haskell-nix/Lobby).

When you're ready to submit a pull request, test it with:
```
git submodule update --init --recursive
nix-shell --run "LANGUAGE_TESTS=yes cabal test"
```

Make sure that all the tests that were passing prior to your PR are still
passing afterwards; it's OK if no new tests are passing.
