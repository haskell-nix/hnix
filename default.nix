{ compiler    ? "ghc822" # "ghc842" also works
, doProfiling ? false
, doBenchmark ? false
, doTracing   ? false
, doStrict    ? false
, rev         ? "9d0b6b9dfc92a2704e2111aa836f5bdbf8c9ba42"
, sha256      ? "096r7ylnwz4nshrfkh127dg8nhrcvgpr69l4xrdgy3kbq049r3nb"
, nixpkgs     ? import (builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    inherit sha256; }) {
    config.allowUnfree = true;
    config.allowBroken = false;
  }
# , nixpkgs     ? import ((import <nixpkgs> {}).fetchFromGitHub {
#     owner = "NixOS"; repo = "nixpkgs"; inherit rev sha256; }) {
}:

let inherit (nixpkgs) pkgs;

  haskellPackages = pkgs.haskell.packages.${compiler}.override {
    overrides = with pkgs.haskell.lib; self: super:
      if compiler == "ghcjs" then {} else
      {
        cryptohash-md5    = doJailbreak super.cryptohash-md5;
        cryptohash-sha1   = doJailbreak super.cryptohash-sha1;
        cryptohash-sha256 = doJailbreak super.cryptohash-sha256;
        cryptohash-sha512 = doJailbreak super.cryptohash-sha512;
        serialise         = dontCheck super.serialise;

        compact =
          if compiler == "ghc842"
          then doJailbreak super.compact
          else super.compact;

        ghc-compact =
          if compiler == "ghc802"
          then super.ghc-compact_0_1_0_0
          else null;

        ghc-datasize =
          if doProfiling
          then null
          else pkgs.haskell.lib.overrideCabal super.ghc-datasize (attrs: {
                 enableLibraryProfiling    = false;
                 enableExecutableProfiling = false;
               });

        ghc-heap-view =
          if doProfiling
          then null
          else pkgs.haskell.lib.overrideCabal super.ghc-heap-view (attrs: {
                 enableLibraryProfiling    = false;
                 enableExecutableProfiling = false;
               });
      };
  };

in haskellPackages.developPackage {
  root = ./.;

  source-overrides =
    if compiler == "ghc802"
    then {
      lens-family-core = "1.2.1";
      lens-family = "1.2.1";
    }
    else {};

  modifier = drv: pkgs.haskell.lib.overrideCabal drv (attrs: {
    testHaskellDepends = attrs.testHaskellDepends ++
      [ pkgs.nix pkgs.haskell.packages.ghc822.hpack ];

    enableLibraryProfiling    = doProfiling;
    enableExecutableProfiling = doProfiling;

    inherit doBenchmark;

    configureFlags =
         pkgs.stdenv.lib.optional doTracing   "--flags=tracing"
      ++ pkgs.stdenv.lib.optional doProfiling "--flags=profiling"
      ++ pkgs.stdenv.lib.optional doStrict    "--ghc-options=-Werror";
  });
}
