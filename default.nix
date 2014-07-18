{ cabal, parsers, trifecta, text, ansiWlPprint, parsec, transformers
, useParsec ? true
}:

cabal.mkDerivation (self: rec {
  pname = "hnix";
  version = "0.0.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  noHaddock = true;
  buildDepends = [
    ansiWlPprint
    text
    transformers
  ] ++ (if useParsec then [ parsec ] else [ parsers trifecta ]);
  meta = {
    homepage = "https://github.com/jwiegley/hnix";
    description = "Haskell implementation of the Nix language";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
