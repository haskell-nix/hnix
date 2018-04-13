# hnix

Haskell parser, evaluator and type checker for the Nix language.

## Prerequisites

Nix is installed and in your `$PATH`. This is so that `nix-store` can be used
for interacting with store paths, until `hnix-store` is ready.

## Getting Started

```bash
$ git clone https://github.com/jwiegley/hnix.git
...
$ cd hnix
$ nix-shell
$ runhaskell Setup.hs configure --enable-tests
$ runhaskell Setup.hs build
$ runhaskell Setup.hs test
# To run all of the tests, which takes up to a minute:
$ LANGUAGE_TESTS=yes NIXPKGS_TESTS=yes runhaskell Setup.hs test
$ runhaskell Setup.hs bench
$ ./dist/build/hnix/hnix --help
```

## Building with profiling enabled

To build `hnix` with profiling enabled:

```
$ nix-shell --arg doProfiling true
$ runhaskell Setup.hs configure --enable-tests --enable-profiling
$ runhaskell Setup.hs build
$ ./dist/build/hnix/hnix <args> +RTS -p
```

## How you can help

If you're looking for a way to help out, try taking a look [here](https://github.com/jwiegley/hnix/issues?q=is%3Aissue+is%3Aopen+label%3A%22help+wanted%22+no%3Aassignee).  When you find an issue that looks interesting to you, comment on the ticket to let others know you're working on it; look for others who might have done the same.  You can talk with everyone live on [gitter](https://gitter.im/haskell-nix/Lobby).

When you're ready to submit a pull request, test it with:
```
git submodule update --init --recursive
nix-shell --run "LANGUAGE_TESTS=yes runhaskell Setup.hs test"
```

Make sure that all the tests that were passing prior to your PR are still passing afterwards; it's OK if no new tests are passing.
