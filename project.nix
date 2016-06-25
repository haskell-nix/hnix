{ mkDerivation, ansi-wl-pprint, base, containers, data-fix, deepseq
, parsers, stdenv, tasty, tasty-hunit, tasty-th, text, transformers
, trifecta, unordered-containers, cabal-install, criterion, pkgs
}:

let
  inherit (builtins) filterSource;
  inherit (pkgs.lib) elem;
in

mkDerivation {
  pname = "hnix";
  version = "0.3.1";
  src = let
    notNamed = list: name: !(elem (baseNameOf name) list);
  in filterSource (n: _: notNamed [".git" "dist" "benchmarks"] n) ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    ansi-wl-pprint base containers data-fix deepseq parsers text
    transformers trifecta unordered-containers cabal-install criterion
  ];
  executableHaskellDepends = [
    ansi-wl-pprint base containers data-fix deepseq
  ];
  testHaskellDepends = [
    base containers data-fix tasty tasty-hunit tasty-th text
  ];
  homepage = "http://github.com/jwiegley/hnix";
  description = "Haskell implementation of the Nix language";
  license = stdenv.lib.licenses.bsd3;
}
