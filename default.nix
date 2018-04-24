{ compiler    ? "ghc822" # "ghc842" also works
, doProfiling ? false
, doBenchmark ? false
, doTracing   ? false
, rev         ? "255a833e841628c0b834575664eae373e28cdc27"
, sha256      ? "022xm1pf4fpjjy69g7qz6rpqnwpjcy1l0vj49m8xmgn553cs42ch"
# , nixpkgs     ? import ((import <nixpkgs> {}).fetchFromGitHub {
#     owner = "NixOS"; repo = "nixpkgs"; inherit rev sha256; }) {
, nixpkgs     ? import (builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    inherit sha256; }) {
    config.allowUnfree = true;
    config.allowBroken = false;
  }
}:

let inherit (nixpkgs) pkgs;

  haskellPackages = pkgs.haskell.packages.${compiler}.override {
    overrides = with pkgs.haskell.lib; self: super: rec {
      compact   = if compiler == "ghc842"
                  then doJailbreak super.compact
                  else super.compact;
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
        # haskellPackages.cabal-install
      ];

    enableLibraryProfiling    = doProfiling;
    enableExecutableProfiling = doProfiling;

    inherit doBenchmark;

    configureFlags = if doTracing
                     then [ "--flags=tracing" ]
                     else [];
  });
}
