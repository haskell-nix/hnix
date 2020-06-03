{ compiler    ? "ghc883"

# doBenchmark: Dependency checking + compilation and execution for benchmarks listed in the package description file.
, doBenchmark ? false
# Generation and installation of a coverage report.
# See https://wiki.haskell.org/Haskell_program_coverage
, doCoverage  ? false
# Generation and installation of haddock API documentation
, doHaddock   ? false
# Nix dependency checking, compilation and execution of test suites listed in the package description file.
, doCheck     ? true
, enableLibraryProfiling ? false
, enableExecutableProfiling ? false
, doTracing   ? false
# Enables GHC optimizations for production use, without optimizations compilation is way faster
, doOptimize  ? false
, doStrict    ? false
# Escape the version bounds from the cabal file. You may want to avoid this function.
, doJailbreak ? false
, enableSharedExecutables ? false
, enableSharedLibraries ? true
, enableStaticLibraries ? false
#  2020-06-02: NOTE: enableDeadCodeElimination = true: On GHC =< 8.8.3 macOS build falls due to https://gitlab.haskell.org/ghc/ghc/issues/17283, so temporarily set default to `false`
, enableDeadCodeElimination ? false
, doHyperlinkSource ? false
, doStrip ? false
, justStaticExecutables ? false
# Don't fail at configure time if there are multiple versions of the same package in the (recursive) dependencies of the package being built. Will delay failures, if any, to compile time.
, allowInconsistentDependencies ? false

, withHoogle  ? true


, useRev ? false
# Accepts Nixpkgs channel name and Git revision
, rev ? "nixpkgs-unstable"

, pkgs ?
    if builtins.compareVersions builtins.nixVersion "2.0" < 0
    then abort "hnix requires at least nix 2.0"
    else
      if useRev
        # Please do not guard with hash, so the package able to use current channels (rolling `rev`) of Haskell&Nixpkgs
        then import (builtins.fetchTarball "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz") {}
        else import <nixpkgs> {}
      // {
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

    testHaskellDepends = attrs.testHaskellDepends ++ [
      pkgs.nix
      haskellPackages.criterion
    ];

    inherit doBenchmark;
    inherit doCoverage;
    inherit doHaddock;
    inherit doCheck;
    inherit enableLibraryProfiling;
    inherit enableExecutableProfiling;
    inherit enableSharedExecutables;
    inherit enableSharedLibraries;
    inherit enableStaticLibraries;
    inherit enableDeadCodeElimination;
    inherit allowInconsistentDependencies;

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
