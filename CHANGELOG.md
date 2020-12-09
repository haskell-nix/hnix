
# Changelog

## [(diff)](https://github.com/haskell-nix/hnix/compare/0.11.0...master#files_bucket) Progress

### [(diff)](https://github.com/haskell-nix/hnix/compare/0.11.0...0.11.1#files_bucket) 0.11.1 (2020-12-09)

* Additional:
  * [(link)](https://github.com/haskell-nix/hnix/commit/d32a6fbaf3df1c8879d1b19a18f21c031a73e56c) `Nix/Builtins`: `isString` fixed - It used to return `True` for values coercible to string like derivations and paths. It only accepts string values now.
  * [(link)](https://github.com/haskell-nix/hnix/commit/53b4db2525a8f074d8c262fa7b66ce97e5820890) `Nix/Builtins`: `substring` fixed - Negative lengths used to capture an empty string. Now they capture the whole rmeainder of the string.
  * [(link)](https://github.com/haskell-nix/hnix/commit/dc31c5e64f8c7aaaea14cac0134bd47544533e67) `Nix/Effects`: `pathExists` fixed - Now also works with directories.
  * [(link)](https://github.com/haskell-nix/hnix/commit/e2ad934492eeac9881527610e4a1c1cf31ea1115) `Nix/Parser`: `->` is now properly right-associative (was non-associative).
  * [(link)](https://github.com/haskell-nix/hnix/commit/50baea5e1e482be3c4fcc13c9a45b1083243f681) `Nix/Parser`: Nix `assert` parser (`nixAssert` function) now accepts top-level Nix format also (which means also accepts all kinds of statements), before that it accepted only regular Nix expressions.
  * [(link)](https://github.com/haskell-nix/hnix/commit/59698de7185dfae508e5ccea4377a82023c4a0d5) `Nix/Render`: `renderLocation` now also shows/handles location of errors in raw strings.


## [(diff)](https://github.com/haskell-nix/hnix/compare/0.10.1...0.11.0#files_bucket) 0.11.0 (2020-11-02)

* Breaking:
  * [(link)](https://github.com/haskell-nix/hnix/pull/740) Deleted incorrect `instance Generic1 NKeyName` from `module Nix.Expr.Types`.
  * [(link)](https://github.com/haskell-nix/hnix/pull/739) Parentheses now are properly included in the location annotation for Nix expressions, change of `nixParens` in `module Nix.Parser` essentially results in the change of all module `nix*` function results, essentially making results of the whole module more proper.

* Additional:
  * [(link)](https://github.com/haskell-nix/hnix/pull/741) Fix QQ Text lifting error: work around of [GHC#12596 "can't find interface-file declaration"](https://gitlab.haskell.org/ghc/ghc/-/issues/12596).
  * [(link)](https://github.com/haskell-nix/hnix/pull/744) Fix comments inclusion into location annotations, by using pre-whitespace position for source end locations.


### [(diff)](https://github.com/haskell-nix/hnix/compare/0.10.0...0.10.1#files_bucket) 0.10.1 (2020-09-13)

* Additional:
  * [(link)](https://github.com/haskell-nix/hnix/pull/715) `{Binding, NExpr, NExprF, NKeyName}` gained `Ord1` instances.
    * These instances were required by downstream projects to be able to use newer HNix.
  * [(link)](https://github.com/haskell-nix/hnix/pull/712) CLI gained `--long-version` option for gathering a detailed debug information.
    * Currently, reports Git commit and its date.
    * [(link)](https://github.com/haskell-nix/hnix/issues/718) Currently does not work in case of use of the `nix-build`, in which case simply returns `UNKNOWN` placeholder.


## [(diff)](https://github.com/haskell-nix/hnix/compare/0.9.1...0.10.0#files_bucket) 0.10.0 (2020-09-12)

* Breaking:
  * [(link)](https://github.com/haskell-nix/hnix/pull/699) Removed `NExpr` `{FromJSON, ToJSON}` instances.
    * This also removed the JSON output feature for unevaluated expression trees.

* Additional:
  * [(link)](https://github.com/haskell-nix/hnix/pull/703) CLI gained `--version` option.
  * Dependencies:
    * [(link)](https://github.com/haskell-nix/hnix/pull/686) Requires last major `data-fix` (`0.3`).
    * [(link)](https://github.com/haskell-nix/hnix/pull/679) Requires last major `prettyprinter` (`1.7`).


### [(diff)](https://github.com/haskell-nix/hnix/compare/0.9.0...0.9.1#files_bucket) 0.9.1 (2020-07-13)

* Additional:
  * REPL:
    * Better tab completion.
    * Accepting multi-line input.
    * Support for passing evaluated expression result of `hnix --eval -E`.
      to REPL as an `input` variable.
    * Support for loading `.hnixrc` from the current directory.
  * Reporting of `builtins.nixVersion` bumped from 2.0 to 2.3.
  * Dependencies:
    * Freed from: `{interpolate, contravariant, semigroups, generic-random, tasty-quickcheck}`.
    * Requires last major `repline` (`0.4`).


## [(diff)](https://github.com/haskell-nix/hnix/compare/0.8.0...0.9.0#files_bucket) 0.9.0 (2020-06-15)

* Breaking:
  * Removed instances due to migration to `haskeline 0.8`:
    * `instance MonadException m => MonadException(StateT(HashMap FilePath NExprLoc) m)`.
    * `instance MonadException m => MonadException(Fix1T StandardTF m)`.
  * Dependencies:
    * Requires last major `haskeline` (`0.8`).

* Additional:
  * Library: Official support for `GHC 8.4 - 8.10`.
  * Executable complies only under `GHC 8.10`.

* Changelog started. Previous release was `0.8.0`.


---

HNix uses [PVP Versioning][1].

[1]: https://pvp.haskell.org
