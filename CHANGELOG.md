
# Changelog

## [(diff)](https://github.com/haskell-nix/hnix/compare/0.12.0...master#files_bucket) Progress


## [(diff)](https://github.com/haskell-nix/hnix/compare/0.11.1...0.12.0#files_bucket) 0.12.0 (2020-12-31)

* *Disclaimer*: Current `derivationStrict` primOp implementation and so every evaluation of a derivation into a store path currently relies on the `hnix-store-remote`, which for those operations relies on the running `nix-daemon`, and so operations use/produce effects into the `/nix/store`. Be cautious - it is effectful.

* Introduction:
  * New module `Nix.Effects.Derivation`.
  * Operations on derivations:
    * old got principled implementations.
    * also new operations got introduced.
  * Implementation of the `derivationStrict` primOp.

* Breaking:
  * [(link)](https://github.com/haskell-nix/hnix/pull/554/3bba5549273c892c60aad5dd6d5058a8db40efbf) `Nix.Effects`: **class** `MonadStore` got principled implementation.
    * `addPath'` got principled into `addToStore`.
    * `toFile_` got principled into `addTextToStore'`.
    * For help & easy migration you may use `addPath` & `toFile_` `addTextToStore` standalone functions in the module.
  * [(link)](https://github.com/haskell-nix/hnix/pull/554/3bba5549273c892c60aad5dd6d5058a8db40efbf) `Nix.Effects.Basic`: `defaultDerivationStrict` got reimplemented & moved into `Nix.Effects.Derivation`.
  * [(link)](https://github.com/haskell-nix/hnix/pull/554/3bba5549273c892c60aad5dd6d5058a8db40efbf) `Nix.Standard`: instance for `MonadStore (Fix1T t m)` got principled accoding to class `MonadStore` changes.
  * [(link)](https://github.com/haskell-nix/hnix/pull/554/3bba5549273c892c60aad5dd6d5058a8db40efbf) `Nix.Fresh.Basic`: instance for `MonadStore (StdIdT m)` got principled.

* Additional:
  * [(link)](https://github.com/haskell-nix/hnix/pull/554/3bba5549273c892c60aad5dd6d5058a8db40efbf) **New module `Nix.Effects.Derivation`**: [HNix(0.12.0):Nix.Effects.Derivation documentation](https://hackage.haskell.org/package/hnix-0.12.0/docs/Nix-Effects-Derivation.html).
  * [(link)](https://github.com/haskell-nix/hnix/pull/554/commits/9bcfbbe88ff0bd8d803296193ee1d8603dc5289e) `Nix.Convert`: Principled `NVPath -> NixString` coercion.
    * In a form of principled `instance FromValue NixString m (NValue' t f m (NValue t f m))`.
  * [(link)](https://github.com/haskell-nix/hnix/pull/554/commits/a8e6d28fdb98a1c34f425c8395338fdabe96becc) `Nix.String`: Allow custom computations inside string contexts.
    * By providing `runWithStringContext{T,}'` methods into the API.
  * [(link)](https://github.com/haskell-nix/hnix/pull/554/commits/e45f7632c51a9657f6e8d54c39fd4d21c466d85f) Includded support for new `base16-bytestring`, which advertices 2x-4x speed increase of its operations.
  * [(link)](https://github.com/haskell-nix/hnix/pull/554/3bba5549273c892c60aad5dd6d5058a8db40efbf) `Nix.Effects`: `addPath` & `toFile_` standalone functions got principled implementation through the internal use of the new `MonadStore` type class implementation.
  * [(link)](https://github.com/haskell-nix/hnix/pull/554/3bba5549273c892c60aad5dd6d5058a8db40efbf) `Nix.Effects`: added `addTextToStore`, `parseStoreResult` implementations.
  * [(link)](https://github.com/haskell-nix/hnix/pull/554/3bba5549273c892c60aad5dd6d5058a8db40efbf) `Nix.Effects`: added type synonyms `{RecursiveFlag, RepairFlag, StorePathName, FilePathFilter, StorePathSet}`.
  * [(link)](https://github.com/haskell-nix/hnix/pull/760) `Nix.Exec`: Fixed the rendering of internal `Frames`.
    * Which is an internal mechanism of a project to passing around messages with their context, still to be used internally).
  * [(link)](https://github.com/haskell-nix/hnix/pull/554/3bba5549273c892c60aad5dd6d5058a8db40efbf) `HNix / Nix`: The library now also uses `hnix-store-remote`.
  * [(link)](https://github.com/haskell-nix/hnix/pull/554/06b0fca9fd607eb2e995f003424e797a41ffa5b7) `cabal.project`: project uses `cryptohash-sha512` override, the `hnix-store-core` requires it from `hnix` and uses that override also. [Detailed info](https://github.com/haskell-hvr/cryptohash-sha512/pull/5#issuecomment-752796913). We promise to attend to this issue, probably by migrating to `cryptonite` in the nearest releases.

Future note: The HNix is a big project. During the initial development and currently the API for simplicity exposes allmost all functions, types, etc. Big open API means a big effort to create/maintain a quite noisy changelog and you parsing through it, and also creates a frequent requirements to mark releases as major and bother you due to some type changes in some parts that may not be used or applicable to be public API.

This year the most gracious API clean-up would happen, we would check and keep open what Hackage projects are using from the API, and the other parts would be open on the request by a way of rapid minor releases. All that is also a work toward splitting the project into several packages some time in the future (split would be into something like Expressions, Evaluation, Executable, Extra), which migration also would be done most thoughful and graceful as possible, with as much easiness and automation for downstream project migrations as possible.

If it is possible, please, try to switch & use the higher-level API functions where it is applicable. Thank you.


### [(diff)](https://github.com/haskell-nix/hnix/compare/0.11.0...0.11.1#files_bucket) 0.11.1 (2020-12-09)

* Additional:
  * [(link)](https://github.com/haskell-nix/hnix/commit/d32a6fbaf3df1c8879d1b19a18f21c031a73e56c) `Nix.Builtins`: `isString` fixed - It used to return `True` for values coercible to string like derivations and paths. It only accepts string values now.
  * [(link)](https://github.com/haskell-nix/hnix/commit/53b4db2525a8f074d8c262fa7b66ce97e5820890) `Nix.Builtins`: `substring` fixed - Negative lengths used to capture an empty string. Now they capture the whole rmeainder of the string.
  * [(link)](https://github.com/haskell-nix/hnix/commit/dc31c5e64f8c7aaaea14cac0134bd47544533e67) `Nix.Effects`: `pathExists` fixed - Now also works with directories.
  * [(link)](https://github.com/haskell-nix/hnix/commit/e2ad934492eeac9881527610e4a1c1cf31ea1115) `Nix.Parser`: `->` is now properly right-associative (was non-associative).
  * [(link)](https://github.com/haskell-nix/hnix/commit/50baea5e1e482be3c4fcc13c9a45b1083243f681) `Nix.Parser`: Nix `assert` parser (`nixAssert` function) now accepts top-level Nix format also (which means also accepts all kinds of statements), before that it accepted only regular Nix expressions.
  * [(link)](https://github.com/haskell-nix/hnix/commit/59698de7185dfae508e5ccea4377a82023c4a0d5) `Nix.Render`: `renderLocation` now also shows/handles location of errors in raw strings.


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
