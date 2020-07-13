# Changelog

## [0.9.1](https://github.com/haskell-nix/hnix/compare/0.9.0...0.9.1) (2020-07-13)

* `builtins.nixVersion` bumped from 2.0 to 2.3
* Documentation improvements for `Nix.{Atoms,Expr.Types}`
* Reduced number of dependencies
* REPL improvements
  * Better tab completion
  * Multi-line input
  * Support for passing evaluated expression result of `hnix --eval -E`
    to REPL as `input` variable.
  * Support for loading `.hnixrc` from current directory

## [0.9.0](https://github.com/haskell-nix/hnix/compare/0.8.0...0.9.0) (2020-06-15)

* Changelog started. Previous release was `0.8.0`. In new release:

* Major breaking:
  * Removed instances due to migration to `haskeline >= 0.8 && < 0.9`:
    * `instance MonadException m => MonadException(StateT(HashMap FilePath NExprLoc) m)`
    * `instance MonadException m => MonadException(Fix1T StandardTF m)`

* Minor:
  * Added support for `GHC 8.10`

---

`HNix` uses [PVP Versioning][1].

[1]: https://pvp.haskell.org
