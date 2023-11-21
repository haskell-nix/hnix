[![Chatroom Gitter](https://img.shields.io/badge/Chatroom-Gitter-%23753a88)](https://gitter.im/haskell-nix/Lobby)
[![Hackage](https://img.shields.io/hackage/v/hnix?color=%235e5086&label=Latest%20release%20on%20Hackage)](https://hackage.haskell.org/package/hnix)
[![Hackage Matrix Builder](https://img.shields.io/badge/Hackage%20Matrix-Builder-%235e5086)](https://matrix.hackage.haskell.org/package/hnix)
[![Bounds](https://img.shields.io/hackage-deps/v/hnix?label=Released%20dep%20bounds)](https://packdeps.haskellers.com/feed?needle=hnix)
[![Hydra CI](https://img.shields.io/badge/Nixpkgs%20Hydra-CI-%234f72bb)](https://hydra.nixos.org/job/nixpkgs/trunk/haskellPackages.hnix.x86_64-linux#tabs-status)
[![Repology page](https://img.shields.io/badge/Repology-page-%23005500)](https://repology.org/project/haskell:hnix/versions)


# HNix

Parser, evaluator and type checker for the Nix language written in Haskell.


## Prerequisites
Tooling is WIP, `nix-shell` and `nix-store` are still used for their purpose, so, to access them Nix is required to be installed.

*Disclaimer*: Since still using Nix for some operations, current `derivationStrict` primOp implementation and so evaluations of a derivation into a store path currently rely on the `hnix-store-remote`, which for those operations relies on the running `nix-daemon`, and so operations use/produce effects into the `/nix/store`. Be cautious - it is effectful (produces `/nix/store` entries).

## Building the project

### Git clone

```shell
git clone 'https://github.com/haskell-nix/hnix.git' && cd hnix
```


### (optional) Cachix prebuild binary caches

If you would use our Nix-shell environment for development, you can connect to our Cachix HNix build caches:

1. Run:
    ```shell
    nix-env -iA cachix -f https://cachix.org/api/v1/install
    ```


2. Run: `cachix use hnix`


### Building with Cabal

Cabal [Quickstart](https://cabal.readthedocs.io/en/3.4/nix-local-build.html).

1. (Optional), to enter the projects reproducible Nix environment:
    ```shell
    nix-shell
    ```
    
2. Building:
    ```shell
    cabal v2-configure
    cabal v2-build
    ```
  
3. Loading the project into `ghci` REPL:
    ```shell
    cabal v2-repl
    ```
    
4. Testing:

  * Default suite:
    ```shell
    cabal v2-test
    ```
  
  * All available tests:
    ```shell
    env ALL_TESTS=yes cabal v2-test
    ```
    
  * Selected (list of tests is in `tests/Main.hs`):
    ```shell
    env NIXPKGS_TESTS=yes PRETTY_TESTS=1 cabal v2-test
    ```

#### Checking the project

##### Benchmarks

To run benchmarks:

```shell
cabal v2-bench
```

##### Profiling

GHC User Manual has a full ["Profiling"](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/profiling.html) section of relevant info.

To build `hnix` with profiling enabled:

```shell
cabal v2-run hnix --enable-profiling --flags=profiling -- <args> +RTS -p
```

Or to put simply:
```shell
# Run profiling for evaluation of a Firefox package.
# Generate:
#  * for all functions
#  * time profiling data
#  * memory allocation profiling data
#  * in the JSON profiling format
cabal v2-run --enable-profiling --flags=profiling --enable-library-profiling --profiling-detail='all-functions' hnix -- --eval --expr '(import <nixpkgs> {}).firefox.outPath' +RTS -Pj

# Then, upload the `hnix.prof` to the https://www.speedscope.app/ to analyze it.
```

"RTS" stands for "RunTime System" and has a lot of options, GHC User Manual has ["Running a compiled program"/"Setting RTS options"](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/runtime_control.html) sections describing them.

##### Full debug info

To run stack traces & full tracing output on `hnix`:

```shell
cabal v2-configure --enable-tests --enable-profiling --flags=profiling --flags=tracing
cabal v2-run hnix -- -v5 --trace <args> +RTS -xc
```

This would give the most information as to what happens during parsing & evaluation.


#### Runing executable

```shell
cabal v2-run hnix -- --help
```
(`--` is for separation between `cabal` & `hnix` args)


### Building with Nix-build

There is a number of build options to use with `nix-build`, documentation of them is in: `./default.nix`, keys essentially pass-through the [Nixpkgs Haskell Lib API](https://github.com/NixOS/nixpkgs/blob/master/pkgs/development/haskell-modules/lib.nix).

Options can be used as:
```shell
nix-build \
  --arg <option1> <argument1> \
  --arg <option2> <argument2> \
  --argstr <option3> "<strinTypeArg>"
```

#### Checking the project
##### Benchmarks

```shell
nix-build \
  --arg disableOptimization false \
  --arg enableDeadCodeElimination true \
  --arg doStrip true \
  --arg doBenchmark true
```

##### Profiling

```shell
nix-build \
  --arg disableOptimization false \
  --arg enableDeadCodeElimination true \
  --arg enableLibraryProfiling true \
  --arg enableExecutableProfiling true

./result/bin/hnix <args> +RTS -p
```

##### Full debug info

```shell
nix-build \
  --arg disableOptimization false \
  --arg enableDeadCodeElimination true \
  --arg doBenchmark true \
  --arg doStrip false \
  --arg enableLibraryProfiling true \
  --arg enableExecutableProfiling true \
  --arg doTracing true \
  --arg enableDWARFDebugging true

./result/bin/hnix -v5 --trace <args> +RTS -xc
```

#### Runing executable

```shell
./result/bin/hnix
```

## Using HNix

See:
```
hnix --help
```

It has a pretty full/good description of the current options.


### Parse & print

To parse a file with `hnix` and pretty print the result:

```shell
hnix file.nix
```

### Evaluating and printing the resulting value

Expression from a file:

```shell
hnix --eval file.nix
```

Expression:

```shell
hnix --eval --expr 'import <nixpkgs> {}'
```

### Evaluating Nixpkgs

Currently, the main high-level goal is to be able to evaluate all of Nixpkgs:

```shell
hnix --eval --expr "import <nixpkgs> {}" --find
```

### Options supported only by HNix

To see value provenance and thunk context:

```shell
hnix -v2 --values --thunk --eval --expr 'import <nixpkgs> {}'
```

To see tracing as the evaluator runs (note that building with `cabal configure --flags=tracing` will produce much more output than this):

```shell
hnix --trace --eval --expr 'import <nixpkgs> {}'
```

To attempt to generate a reduced test case demonstrating an error:

```shell
hnix --reduce bug.nix --eval --expr 'import <nixpkgs> {}'
```

### REPL

To enter REPL:
```shell
hnix --repl
```

Evaluate an expression and load it into REPL:
```shell
hnix --eval --expr '(import <nixpkgs> {}).pkgs.hello' --repl
```
This binds the evaluated expression result to the `input` variable, so that variable can be inspected.

Use the `:help` command for a list of all available REPL commands.

#### Language laziness

Nix is a lazy language with the ability of recursion, so by default REPL and eval prints are lazy:

```shell
hnix \
  --eval \
  --expr '{ x = true; }'
  
{ x = "<expr>"; }
```

To disable laziness add the `--strict` to commands or `:set strict` in the REPL.

```shell
hnix \
  --eval \
  --strict \
  --expr '{ x = true; }'
  
{ x = true; }
```


## Contributing

* The Haskell Language Server (HLS) works great with our project.

* [Design of the HNix code base Wiki article](https://github.com/haskell-nix/hnix/wiki/Design-of-the-HNix-code-base).

1. If something in the [quests](https://github.com/haskell-nix/hnix/issues?q=is%3Aissue+is%3Aopen+label%3A%22help+wanted%22+no%3Aassignee) looks interesting, look through the thread and leave a comment taking it, to let others know you're working on it.

2. You are free to chat with everyone on [Gitter](https://gitter.im/haskell-nix/Lobby).

3. When the pull request is ready to be submitted, to save time - please, test it with:
    
    ```shell
    cabal v2-test
    ```
    
    Please, check that all default tests that were passing prior are still passing. It's OK if no new tests are passing.
    
    
### (optional) Minimalistic development status loop with amazing [`ghcid`](https://github.com/ndmitchell/ghcid)

If HLS is not your cup of yea:

```shell
ghcid --command="cabal v2-repl --repl-options=-fno-code --repl-options=-fno-break-on-exception --repl-options=-fno-break-on-error --repl-options=-v1 --repl-options=-ferror-spans --repl-options=-j"
```
(optional) To use projects reproducible environment, wrap `ghcid ...` command into a `nix-shell --command ' '`.

For simplicity `alias` the command in your shell.


## Current status

To understand the project implementation state see:
  * [ChangeLog](https://github.com/haskell-nix/hnix/blob/master/ChangeLog.md)
  * [Opened reports](https://github.com/haskell-nix/hnix/issues)
  * [Project status](https://github.com/haskell-nix/hnix/wiki/Project-status)
  * [Design of the HNix code base](https://github.com/haskell-nix/hnix/wiki/Design-of-the-HNix-code-base)

