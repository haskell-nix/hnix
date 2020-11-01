attrs@
  { compiler ? "ghc8101"
  , withHoogle ? true
  }:
(import ./. attrs).env
