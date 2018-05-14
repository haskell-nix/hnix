{ compiler    ? "ghc822"
, doProfiling ? false
, doBenchmark ? false
, doTracing   ? false
, doStrict    ? false
, rev         ? "a2981671523416ad5c9a49de8c16ac8fccb2306b"
, sha256      ? "1cgp63hdzfs836ynvfsknp43dkgq0dfny1bb79c97rzh1alqx0kz"
, nixpkgs     ? if builtins.compareVersions builtins.nixVersion "2.0" < 0
                then abort "hnix requires nix 2.0"
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
