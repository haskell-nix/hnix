
# Changelog

### [(diff)](https://github.com/haskell-nix/hnix/compare/0.10.0...0.10.1#files_bucket) 0.10.1 (2020-09-13)

* Additional:
  * [(link)](https://github.com/haskell-nix/hnix/pull/715/files) `{Binding, NExpr, NExprF, NKeyName}` gained `Ord1` instances.
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
