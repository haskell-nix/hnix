[![Chatroom Gitter](https://img.shields.io/badge/Chatroom-Gitter-%23753a88)](https://gitter.im/haskell-nix/Lobby)
[![Hackage](https://img.shields.io/hackage/v/hnix?color=%235e5086&label=Latest%20release%20on%20Hackage)](https://hackage.haskell.org/package/hnix)
[![Hackage Matrix Builder](https://img.shields.io/badge/Hackage%20Matrix-Builder-%235e5086)](https://matrix.hackage.haskell.org/package/hnix)
[![Bounds](https://img.shields.io/hackage-deps/v/hnix?label=Released%20dep%20bounds)](https://packdeps.haskellers.com/feed?needle=hnix)
[![Hydra CI](https://img.shields.io/badge/Nixpkgs%20Hydra-CI-%234f72bb)](https://hydra.nixos.org/job/nixpkgs/trunk/haskellPackages.hnix.x86_64-linux#tabs-status)
[![Repology page](https://img.shields.io/badge/Repology-page-%23005500)](https://repology.org/project/haskell:hnix/versions)


# hnix

Parser, evaluator and type checker for the Nix language written in Haskell.


## Contents

<!-- TOC generates automatically, do not bother editing any further TOC text -->
<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->


- [Prerequisites](#prerequisites)
- [Getting Started](#getting-started)
  - [Cachix prebuild binary caches](#cachix-prebuild-binary-caches)
  - [Development using Cabal](#development-using-cabal)
    - [Building the project](#building-the-project)
      - [With benchmarks](#with-benchmarks)
      - [With profiling](#with-profiling)
      - [With full debug info](#with-full-debug-info)
    - [Run HNix](#run-hnix)
  - [Use of the Nix-build](#use-of-the-nix-build)
    - [Run benchmarks](#run-benchmarks)
    - [With profiling](#with-profiling-1)
    - [With full debug info](#with-full-debug-info-1)
    - [Run the result](#run-the-result)
- [Development status loop with amazing `ghcid`](#development-status-loop-with-amazing-ghcid)
- [Using the HNix REPL](#using-the-hnix-repl)
- [Contributing](#contributing)
- [Evaluating Nixpkgs with HNix](#evaluating-nixpkgs-with-hnix)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->


## Prerequisites
Tooling is WIP, `nix-shell` and `nix-store` are still used for their purpose, so, to access them Nix is required to be installed.

## Getting Started

```
# Note: --recursive
git clone --recursive https://github.com/haskell-nix/hnix.git
cd hnix
```


### Cachix prebuild binary caches

If you opt in to use of Nix environment, please enable the official HNix Cachix binary cache:

1. Go through https://cachix.org/ and set it up.

2. Run: `cachix use hnix`


### Development using Cabal

Cabal [Quickstart](https://cabal.readthedocs.io/en/3.4/nix-local-build.html).

1. (Optional), to enter the projects reproducible Nix environment:
```
nix-shell
```
  
2. Building:
```
cabal v2-configure
cabal v2-build
```

3. Loading the project into `ghci` REPL:
```
cabal v2-repl
```

4. Testing:

* Default suite:
```
cabal v2-test
```

* All available tests:
```
env ALL_TESTS=yes cabal v2-test
```

* Selected (list of tests is in `tests/Main.hs`):
```
env NIXPKGS_TESTS=yes PRETTY_TESTS=1 cabal v2-test
```

#### Building the project

##### With benchmarks

To run benchmarks:

```
cabal v2-bench
```

##### With profiling

To build `hnix` with profiling enabled:

```
cabal v2-configure --enable-tests --enable-profiling --flags=profiling
cabal v2-run hnix -- <args> +RTS -p
```

##### With full debug info

To build `hnix` for debugging, with full tracing output and stack traces:

```
cabal v2-configure --enable-tests --enable-profiling --flags=profiling --flags=tracing
cabal v2-run hnix -- -v5 --trace <args> +RTS -xc
```

Note that this going to run quite slowly, but would give the most information as to what happens during parsing & evaluation.


#### Run HNix
```
cabal v2-run hnix -- --help
```
(`--` is for separation between `cabal` & `hnix` args)


### Use of the Nix-build

There is a number of build options to use with `nix-build`, documentation of them is in: `./default.nix`, keys essentially pass-through the [Nixpkgs Haskell Lib API](https://github.com/NixOS/nixpkgs/blob/master/pkgs/development/haskell-modules/lib.nix).

Options can be used as:
```
nix-build \
  --arg <option1> <argument1> \
  --arg <option2> <argument2> \
  --argstr <option3> "<strinTypeArg>"
```

#### Run benchmarks

```
nix-build \
  --arg disableOptimization false \
  --arg enableDeadCodeElimination true \
  --arg doStrip true \
  --arg doBenchmark true
```

#### With profiling

```
nix-build \
  --arg disableOptimization false \
  --arg enableDeadCodeElimination true \
  --arg enableLibraryProfiling true \
  --arg enableExecutableProfiling true
```

#### With full debug info

```
nix-build \
  --arg disableOptimization false \
  --arg enableDeadCodeElimination true \
  --arg doBenchmark true \
  --arg doStrip false \
  --arg enableLibraryProfiling true \
  --arg enableExecutableProfiling true
  --arg doTracing true \
  --arg enableDWARFDebugging true
```

#### Run the result

```
./result/bin/hnix
```

## Development status loop with amazing [`ghcid`](https://github.com/ndmitchell/ghcid)

```
ghcid --command="cabal v2-repl --repl-options=-fno-code --repl-options=-fno-break-on-exception --repl-options=-fno-break-on-error --repl-options=-v1 --repl-options=-ferror-spans --repl-options=-j"
```
(optional) To use projects reproducible environment, wrap `ghcid ...` command into a `nix-shell --command ' '`.

For simplicity `alias` the command in your shell.


## Using the HNix REPL

Enter in:
```
hnix --repl
```

Evaluate an expression:
```
hnix --eval -E '(import <nixpkgs> {}).pkgs.hello' --repl
```
This also binds the evaluated expression result to the `input` variable, so that variable can be inspected.

Use the `:help` command for a list of all available REPL commands.


## Contributing

1. If something in the [quests](https://github.com/haskell-nix/hnix/issues?q=is%3Aissue+is%3Aopen+label%3A%22help+wanted%22+no%3Aassignee) looks interesting, look through the thread and leave a comment taking it, to let others know you're working on it.

2. You are free to chat with everyone on [Gitter](https://gitter.im/haskell-nix/Lobby).

3. When the pull request is ready to be submitted, to save time - please, test it with:

```
git submodule update --init --recursive
nix-shell --run "LANGUAGE_TESTS=yes cabal v2-test"
```

Please, check that all tests that were passing prior (most probably all tests mentioned in the command) are still passing for the PR, it is faster to check that locally than through CI. It's OK if no new tests are passing.

## Evaluating Nixpkgs with HNix

Currently, the main high-level goal is to be able to evaluate all of Nixpkgs:

```
hnix --eval -E "import <nixpkgs> {}" --find
```
