{ compiler    ? "ghc822"

, doProfiling ? false
, doBenchmark ? false
, doTracing   ? false
, doStrict    ? false

, rev     ? "255a833e841628c0b834575664eae373e28cdc27"
, sha256  ? "022xm1pf4fpjjy69g7qz6rpqnwpjcy1l0vj49m8xmgn553cs42ch"
, nixpkgs ?
    if builtins.compareVersions builtins.nixVersion "2.0" < 0
    then abort "hnix requires at least nix 2.0"
    else import (builtins.fetchTarball {
           url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
           inherit sha256; }) {
           config.allowUnfree = true;
           config.allowBroken = false;
         }
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

  overrides = with pkgs.haskell.lib; self: super:
    if compiler == "ghc802"
    then {
      concurrent-output = doJailbreak super.concurrent-output;
    }
    else {};

  source-overrides =
    if compiler == "ghc802"
    then {
      lens-family-core = "1.2.1";
      lens-family = "1.2.1";
    }
    else {};

  modifier = drv: pkgs.haskell.lib.overrideCabal drv (attrs: {
    testHaskellDepends = attrs.testHaskellDepends ++
      [ pkgs.nix pkgs.haskell.packages.ghc822.hpack

        (let cabalInstallVersion = {
               ghc802 = "1.24.0.2";
               ghc822 = "2.0.0.1";
               ghc842 = "2.2.0.0";
             }; in
         haskellPackages.callHackage "cabal-install"
          cabalInstallVersion.${compiler} {})
      ];

    enableLibraryProfiling    = doProfiling;
    enableExecutableProfiling = doProfiling;

    inherit doBenchmark;

    configureFlags =
         pkgs.stdenv.lib.optional doTracing   "--flags=tracing"
      ++ pkgs.stdenv.lib.optional doProfiling "--flags=profiling"
      ++ pkgs.stdenv.lib.optional doStrict    "--ghc-options=-Werror";
  });
}
