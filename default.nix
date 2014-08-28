{ cabal, parsers, trifecta, text, ansiWlPprint, parsec, transformers
, tasty, tastyHunit, tastyTh, unorderedContainers
, useParsec ? true
}:

cabal.mkDerivation (self: rec {
  pname = "hnix";
  version = "0.0.1";
  src = builtins.filterSource (path: type: type != "unknown") ./.;
  isLibrary = true;
  isExecutable = true;
  noHaddock = true;
  buildDepends = [
    ansiWlPprint text transformers parsers
  ] ++ (if useParsec then [ parsec ] else [ trifecta ]);
  testDepends = [
    tasty tastyHunit tastyTh unorderedContainers
  ];
  meta = {
    homepage = "https://github.com/jwiegley/hnix";
    description = "Haskell implementation of the Nix language";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
