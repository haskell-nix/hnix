# Changelog

General:
  * Binary builds only when `GHC >= 8.10` due to easier `haskeline >= 0.8 && < 0.9`.

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
