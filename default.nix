{ compiler    ? "ghc822"
, doProfiling ? false
, doBenchmark ? false
, rev         ? "ee28e35ba37ab285fc29e4a09f26235ffe4123e2"
, sha256      ? "0a6xrqjj2ihkz1bizhy5r843n38xgimzw5s2mfc42kk2rgc95gw5"
, nixpkgs     ? import (builtins.fetchTarball {
    url    = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    sha256 = sha256; }) {
    config.allowBroken = false;
    config.allowUnfree = true;
  }
}:

let inherit (nixpkgs) pkgs;

  haskellPackages = pkgs.haskell.packages.${compiler}.override {
    overrides = with pkgs.haskell.lib; self: super: rec {
      serialise = dontCheck super.serialise;
    };
  };

in haskellPackages.developPackage {
  root = ./.;

  source-overrides = {
  };

  modifier = drv: pkgs.haskell.lib.overrideCabal drv (attrs: {
    testHaskellDepends = attrs.testHaskellDepends ++
      [
        pkgs.nix
        haskellPackages.hpack
        haskellPackages.cabal-install
      ];

    enableLibraryProfiling    = doProfiling;
    enableExecutableProfiling = doProfiling;

    inherit doBenchmark;
  });
}
