# Changelog

## [0.10.0](https://github.com/haskell-nix/hnix/compare/0.9.1...0.10.0)

* [Add `--version` option](https://github.com/haskell-nix/hnix/pull/703)

* [Remove `ToJSON` and `FromJSON` instances for `NExpr`](https://github.com/haskell-nix/hnix/pull/699)
  * This also removes the JSON output feature for unevaluated expression trees.

* [Update `data-fix` dependency to `>= 0.3.0 && < 0.4`](https://github.com/haskell-nix/hnix/pull/686)

* [Update `prettyprinter` dependency to `= 1.7.0 && < 1.8`](https://github.com/haskell-nix/hnix/pull/679)

## [0.9.1](https://github.com/haskell-nix/hnix/compare/0.9.0...0.9.1) (2020-07-13)

* Additional:
  * REPL improvements
    * Better tab completion
    * Multi-line input
    * Support for passing evaluated expression result of `hnix --eval -E`
      to REPL as `input` variable.
    * Support for loading `.hnixrc` from current directory
  * Reporting of `builtins.nixVersion` bumped from 2.0 to 2.3
  * Dependencies:
    * Freed from: `interpolate`, `contravariant`, `semigroups`, `generic-random`, `tasty-quickcheck`
    * `repline >= 0.4.0.0 && < 0.5`

## [0.9.0](https://github.com/haskell-nix/hnix/compare/0.8.0...0.9.0) (2020-06-15)

* Changelog started. Previous release was `0.8.0`. In new release:

* Major breaking:
  * Removed instances due to migration to `haskeline >= 0.8 && < 0.9`:
    * `instance MonadException m => MonadException(StateT(HashMap FilePath NExprLoc) m)`
    * `instance MonadException m => MonadException(Fix1T StandardTF m)`

* Additional:
  * Library: Official support for `GHC 8.4 - 8.10`

---

`HNix` uses [PVP Versioning][1].

[1]: https://pvp.haskell.org
