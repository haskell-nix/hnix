{ compiler    ? "ghc883"

# Don't fail at configure time if there are multiple versions of the same package in the (recursive) dependencies of the package being built. Will delay failures, if any, to compile time.
, allowInconsistentDependencies ? false
# Escape the version bounds from the cabal file. You may want to avoid this function.
, doJailbreak ? false
# Nix dependency checking, compilation and execution of test suites listed in the package description file.
, doCheck     ? true
# Just produce a SDist src tarball
, sdistTarball ? false
# Produce SDist tarball and build project from it
, buildFromSdist ? true
, failOnAllWarnings ? false
# `failOnAllWarnings` + `buildFromSdist`
, buildStrictly ? false
#  2020-06-02: NOTE: enableDeadCodeElimination = true: On GHC =< 8.8.3 macOS build falls due to https://gitlab.haskell.org/ghc/ghc/issues/17283
# Disable GHC code optimizations for faster dev loops. Enable optimizations for production use or benchmarks.
, enableDeadCodeElimination ? false
# Optimization disabled for faster compiling/dev loop
, disableOptimization ? true
# Use faster `gold` ELF linker from GNU binutils instead of older&slower but more versatile GNU linker. Is not available by default since macOS does not have it.
, linkWithGold ? false
# Provide an inventory of performance events and timings for the execution. Provides informaiton in an absolute sense. Nothing is timestamped.
, enableLibraryProfiling ? false
, enableExecutableProfiling ? false
# Include tracing information & abilities. Tracing records the chronology, often with timestamps and is extensive in time
, doTracing   ? false
# Include DWARF debugging information & abilities
, enableDWARFDebugging ? true
# Strip results from all debugging symbols
, doStrip ? false
#	Generate hyperlinked source code for documentation using HsColour, and have Haddock documentation link to it.
, doHyperlinkSource ? false
# Nixpkgs expects shared libraries
, enableSharedLibraries ? true
# Ability to make static libraries
, enableStaticLibraries ? false
# Make hybrid executable that is also a shared library
, enableSharedExecutables ? false
# link executables statically against haskell libs to reduce closure size
, justStaticExecutables ? false
, enableSeparateBinOutput ? false
# Add a post-build check to verify that dependencies declared in the .cabal file are actually used.
, checkUnusedPackages ? false
# Generation and installation of haddock API documentation
, doHaddock   ? false
# Generation and installation of a coverage report. See https://wiki.haskell.org/Haskell_program_coverage
, doCoverage  ? false
# doBenchmark: Dependency checking + compilation and execution for benchmarks listed in the package description file.
, doBenchmark ? false
# Modify a Haskell package to add shell completion scripts for the given executable produced by it. These completion scripts will be picked up automatically if the resulting derivation is installed
, generateOptparseApplicativeCompletion ? false

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

  listOfSetsOfSwitchExtend =
    [
      {
        switch = sdistTarball;
        function = pkgs.haskell.lib.sdistTarball;
      }
      {
        switch = buildFromSdist;
        function = pkgs.haskell.lib.buildFromSdist;
      }
      {
        switch = buildStrictly;
        function = pkgs.haskell.lib.buildStrictly;
      }
      {
        switch = disableOptimization;
        function = pkgs.haskell.lib.disableOptimization;
      }
      {
        switch = doJailbreak;
        function = pkgs.haskell.lib.doJailBreak;
      }
      {
        switch = doStrip;
        function = pkgs.haskell.lib.doStrip;
      }
      {
        switch = enableDWARFDebugging;
        function = pkgs.haskell.lib.enableDWARFDebugging;
      }
      {
        switch = linkWithGold;
        function = pkgs.haskell.lib.linkWithGold;
      }
      {
        switch = failOnAllWarnings;
        function = pkgs.haskell.lib.failOnAllWarnings;
      }
      {
        switch = justStaticExecutables;
        function = pkgs.haskell.lib.justStaticExecutables;
      }
      {
        switch = checkUnusedPackages;
        function = pkgs.haskell.lib.checkUnusedPackages {};
      }
      {
        switch = generateOptparseApplicativeCompletion;
        function = pkgs.haskell.lib.generateOptparseApplicativeCompletion "hnix";
      }
      {
        switch = doHyperlinkSource;
        function = pkgs.haskell.lib.doHyperlinkSource;
      }
    ];

  funcOnSwitchAppliesFunction = set: object:
    if set.switch
      then set.function object
      else object;

  # General description of package
  package = haskellPackages.developPackage {
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

      # Declare that the header set arguments as according Haskell.lib switches
      inherit allowInconsistentDependencies;
      inherit doCheck;
      inherit enableDeadCodeElimination;
      inherit enableLibraryProfiling;
      inherit enableExecutableProfiling;
      inherit enableSharedLibraries;
      inherit enableStaticLibraries;
      inherit enableSharedExecutables;
      inherit enableSeparateBinOutput;
      inherit doBenchmark;
      inherit doCoverage;
      inherit doHaddock;

      configureFlags = pkgs.stdenv.lib.optional doTracing  "--flags=tracing";

      passthru = {
        nixpkgs = pkgs;
        inherit haskellPackages;
      };
    });

    returnShellEnv = false;
  };

  composedPackage = pkgs.lib.foldr (funcOnSwitchAppliesFunction) package listOfSetsOfSwitchExtend;

in composedPackage


