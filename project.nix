{ mkDerivation, ansi-wl-pprint, base, containers, data-fix, parsers
, stdenv, tasty, tasty-hunit, tasty-th, text, transformers
, trifecta, unordered-containers, cabal-install
}:
mkDerivation {
  pname = "hnix";
  version = "0.2.2";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  buildDepends = [
    ansi-wl-pprint base containers data-fix parsers text transformers
    trifecta unordered-containers cabal-install
  ];
  testDepends = [
    base containers data-fix tasty tasty-hunit tasty-th text
  ];
  homepage = "http://github.com/jwiegley/hnix";
  description = "Haskell implementation of the Nix language";
  license = stdenv.lib.licenses.bsd3;
}
