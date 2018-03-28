{ mkDerivation, ansi-wl-pprint, base, containers, criterion
, data-fix, deepseq, deriving-compat, directory, filepath, Glob
, parsers, regex-tdfa, regex-tdfa-text, semigroups, split, stdenv
, tasty, tasty-hunit, tasty-th, text, transformers, trifecta
, unordered-containers, these, optparse-applicative
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
    trifecta unordered-containers these
  ];
  executableHaskellDepends = [
    ansi-wl-pprint base containers data-fix deepseq optparse-applicative
    text
  ];
  testHaskellDepends = [
    base containers data-fix directory filepath Glob split tasty
    tasty-hunit tasty-th text
  ];
  benchmarkHaskellDepends = [ base containers criterion text ];
  homepage = "http://github.com/jwiegley/hnix";
  description = "Haskell implementation of the Nix language";
  license = stdenv.lib.licenses.bsd3;
}
