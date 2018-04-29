{ compiler    ? "ghc822" # "ghc842" also works
, doProfiling ? false
, doBenchmark ? false
, doTracing   ? false
, doStrict    ? false
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
      serialise = dontCheck super.serialise;
      compact =
        if compiler == "ghc842"
        then doJailbreak super.compact
        else super.compact;
      ghc-datasize =
        pkgs.haskell.lib.overrideCabal super.ghc-datasize (attrs: {
          enableLibraryProfiling    = false;
          enableExecutableProfiling = false;
        });
      ghc-heap-view =
        pkgs.haskell.lib.overrideCabal super.ghc-heap-view (attrs: {
          enableLibraryProfiling    = false;
          enableExecutableProfiling = false;
        });
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

    configureFlags =
         pkgs.stdenv.lib.optional doTracing "--flags=tracing"
      ++ pkgs.stdenv.lib.optional doStrict  "--ghc-options=-Werror";
  });
}
