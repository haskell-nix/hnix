{ compiler    ? "ghc865"

, doBenchmark ? false
, doTracing   ? false
, doOptimize  ? false # enables GHC optimizations for production use
, doProfiling ? false # enables profiling support in GHC
, doStrict    ? false

, withHoogle  ? true

, rev ? "8da81465c19fca393a3b17004c743e4d82a98e4f"

, pkgs ?
    if builtins.compareVersions builtins.nixVersion "2.0" < 0
    then abort "hnix requires at least nix 2.0"
    else import (builtins.fetchGit {
           url = "https://github.com/NixOS/nixpkgs/";
           inherit rev; }) {
      config.allowUnfree = true;
      config.allowBroken = true;
      # config.packageOverrides = pkgs: rec {
      #   nix = pkgs.nixStable.overrideDerivation (attrs: with pkgs; rec {
      #     src = if builtins.pathExists ./data/nix/.version
      #           then data/nix
      #           else throw "data/nix doesn't seem to contain the nix source. You may want to run git submodule update --init.";

      #     enableParallelBuilding = true;

      #     configureFlags =
      #       [ "--disable-doc-gen"
      #         "--enable-gc"
      #       ];

      #     buildInputs =
      #       [ bison
      #         flex
      #         autoconf-archive
      #         autoreconfHook
      #         curl
      #         bzip2 xz brotli editline
      #         openssl pkgconfig sqlite boehmgc
      #         boost
      #         git
      #         mercurial
      #       ]
      #       ++ lib.optionals stdenv.isLinux [libseccomp utillinuxMinimal]
      #       ++ lib.optional (stdenv.isLinux || stdenv.isDarwin) libsodium
      #       ++ lib.optional (stdenv.isLinux || stdenv.isDarwin)
      #            (aws-sdk-cpp.override {
      #               apis = ["s3" "transfer"];
      #               customMemoryManagement = false;
      #             });

      #     outputs = builtins.filter (s: s != "doc" && s != "man" ) attrs.outputs;
      #   });
      # };
    }

, mkDerivation   ? null
}:

let
  hnix-store-src = pkgs.fetchFromGitHub {
    owner = "haskell-nix";
    repo = "hnix-store";
    rev = "0.1.0.0";
    sha256 = "1z48msfkiys432rkd00fgimjgspp98dci11kgg3v8ddf4mk1s8g0";
  };

  overlay = pkgs.lib.foldr pkgs.lib.composeExtensions (_: _: {}) [
    (import "${hnix-store-src}/overlay.nix")
    (self: super: with pkgs.haskell.lib; {

      # Type error in the tests under ghc844 package set
      Diff = dontCheck super.Diff;

      semialign         = super.semialign_1_1;

    } // pkgs.lib.optionalAttrs withHoogle {
      ghc = super.ghc // { withPackages = super.ghc.withHoogle; };
      ghcWithPackages = self.ghc.withPackages;
    })
  ];

  overrideHaskellPackages = orig: {
    buildHaskellPackages =
      orig.buildHaskellPackages.override overrideHaskellPackages;
    overrides = if orig ? overrides
      then pkgs.lib.composeExtensions orig.overrides overlay
      else overlay;
  };

  haskellPackages = pkgs.haskell.packages.${compiler}.override
    overrideHaskellPackages;

in haskellPackages.developPackage {
  name = "hnix";
  root = ./.;

  modifier = drv: pkgs.haskell.lib.overrideCabal drv (attrs: {
    buildTools = (attrs.buildTools or []) ++ [
      haskellPackages.cabal-install
      # haskellPackages.brittany
    ];

    enableLibraryProfiling = doProfiling;
    enableExecutableProfiling = doProfiling;

    testHaskellDepends = attrs.testHaskellDepends ++ [
      pkgs.nix
      haskellPackages.criterion
    ];

    inherit doBenchmark;

    configureFlags =
         pkgs.stdenv.lib.optional doTracing  "--flags=tracing"
      ++ pkgs.stdenv.lib.optional doOptimize "--flags=optimize"
      ++ pkgs.stdenv.lib.optional doStrict   "--ghc-options=-Werror";

    passthru = {
      nixpkgs = pkgs;
      inherit haskellPackages;
    };
  });

  returnShellEnv = false;
}
