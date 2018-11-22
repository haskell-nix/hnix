{ compiler ? "ghc844"

, doBenchmark ? false
, doTracing   ? false
# enables GHC optimizations for production use
, doOptimize ? false 
# enables profiling support in GHC
, doProfiling  ? false
, doStrict    ? false

, rev     ? "3f3f6021593070330091a4a2bc785f6761bbb3c1"
, sha256  ? "1a7vvxxz8phff51vwsrdlsq5i70ig5hxvvb7lkm2lgwizgvpa6gv"

, pkgs    ?
    if builtins.compareVersions builtins.nixVersion "2.0" < 0
    then abort "hnix requires at least nix 2.0"
    else import (builtins.fetchTarball {
           url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
           inherit sha256; }) {
           config.allowUnfree = true;
           config.allowBroken = false;
         }

, returnShellEnv ? pkgs.lib.inNixShell
, mkDerivation ? null
}:

let haskellPackages = pkgs.haskell.packages.${compiler};

drv = haskellPackages.developPackage {
  root = ./.;

  overrides = with pkgs.haskell.lib; self: super: {
    mono-traversable = dontCheck super.mono-traversable;
    megaparsec = super.callHackage "megaparsec" "7.0.4" {};
  }
  //
  (pkgs.lib.optionalAttrs (compiler == "ghc802") {
     concurrent-output = doJailbreak super.concurrent-output;
  })
  //
  (pkgs.lib.optionalAttrs (compiler != "ghcjs") {
     cryptohash-md5    = doJailbreak super.cryptohash-md5;
     cryptohash-sha1   = doJailbreak super.cryptohash-sha1;
     cryptohash-sha256 = doJailbreak super.cryptohash-sha256;
     cryptohash-sha512 = doJailbreak super.cryptohash-sha512;
     serialise         = dontCheck super.serialise;

     ghc-datasize =
       overrideCabal super.ghc-datasize (attrs: {
         enableLibraryProfiling    = false;
         enableExecutableProfiling = false;
       });

     ghc-heap-view =
       overrideCabal super.ghc-heap-view (attrs: {
         enableLibraryProfiling    = false;
         enableExecutableProfiling = false;
       });
  });

  source-overrides = pkgs.lib.optionalAttrs (compiler == "ghc802") {
    lens-family-core = "1.2.1";
    lens-family = "1.2.1";
  };

  modifier = drv: pkgs.haskell.lib.overrideCabal drv (attrs: {
    buildTools = (attrs.buildTools or []) ++ [
      pkgs.haskell.packages.${compiler}.cabal-install
    ];

    enableLibraryProfiling = doProfiling;
    enableExecutableProfiling = doProfiling;

    testHaskellDepends = attrs.testHaskellDepends ++
      [ pkgs.nix
        pkgs.haskell.packages.ghc844.criterion
      ];

    inherit doBenchmark;

    configureFlags =
         pkgs.stdenv.lib.optional  doTracing   "--flags=tracing"
      ++ pkgs.stdenv.lib.optional  doOptimize  "--flags=optimize"
      ++ pkgs.stdenv.lib.optional  doStrict    "--ghc-options=-Werror";

    passthru = {
      nixpkgs = pkgs;
    };
  });

  inherit returnShellEnv;
};

in drv
