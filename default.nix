{ mkDerivation, ansi-wl-pprint, base, containers, criterion
, data-fix, deepseq, deriving-compat, parsers, regex-tdfa
, regex-tdfa-text, semigroups, stdenv, tasty, tasty-hunit, tasty-th
, text, transformers, trifecta, unordered-containers
}:
mkDerivation {
  pname = "hnix";
  version = "0.4.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    ansi-wl-pprint base containers data-fix deepseq deriving-compat
    parsers regex-tdfa regex-tdfa-text semigroups text transformers
    trifecta unordered-containers
  ];
  executableHaskellDepends = [
    ansi-wl-pprint base containers data-fix deepseq
  ];
  testHaskellDepends = [
    base containers data-fix tasty tasty-hunit tasty-th text
  ];
  benchmarkHaskellDepends = [ base containers criterion text ];
  homepage = "http://github.com/jwiegley/hnix";
  description = "Haskell implementation of the Nix language";
  license = stdenv.lib.licenses.bsd3;
}
