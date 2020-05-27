{ compiler    ? "ghc865"

, doBenchmark ? false
, doTracing   ? false
, doOptimize  ? false # enables GHC optimizations for production use
, doProfiling ? false # enables profiling support in GHC
, doStrict    ? false

, withHoogle  ? true

, rev ? "29d57de30101b51b016310ee51c2c4ec762f88db" #  2020-05-23: NOTE: UTC 17:00

, sha256 ? "1wjljkffb3gzdvpfc4v98mrhzack6k9i7860n8cf5nipyab6jbq9"

, pkgs ?
    if builtins.compareVersions builtins.nixVersion "2.0" < 0
    then abort "hnix requires at least nix 2.0"
    else import (builtins.fetchTarball {
           url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
           inherit sha256; }) {
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

  #  2020-05-23: NOTE: Currently HNix-store needs no overlay
  # hnix-store-src = pkgs.fetchFromGitHub {
  #   owner = "haskell-nix";
  #   repo = "hnix-store";
  #   rev = "0.2.0.0";
  #   sha256 = "1qf5rn43d46vgqqgmwqdkjh78rfg6bcp4kypq3z7mx46sdpzvb78";
  # };

  overlay = pkgs.lib.foldr pkgs.lib.composeExtensions (_: _: {}) [
    # (import "${hnix-store-src}/overlay.nix")
    (self: super: with pkgs.haskell.lib;
      pkgs.lib.optionalAttrs withHoogle {
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
