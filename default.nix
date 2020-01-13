{ compiler ? "ghc865"

, doBenchmark ? false
, doTracing   ? false
, doOptimize  ? false # enables GHC optimizations for production use
, doProfiling ? false # enables profiling support in GHC
, doStrict    ? false

, withHoogle  ? true

, rev  ? "7e8454fb856573967a70f61116e15f879f2e3f6a"

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

      # These packages only depend on contravariant when ghc >= 8.6.3
      # Without normalizing the dependencies, our build fails with
      # aeson and base-compat-batteries unable to find `contravariant`
      aeson                 = addBuildDepend super.aeson self.contravariant;
      base-compat-batteries = addBuildDepend super.base-compat-batteries self.contravariant;

      mono-traversable  = dontCheck super.mono-traversable;
      regex-tdfa-text   = doJailbreak super.regex-tdfa-text;
      these             = doJailbreak super.these;
      semialign         = super.semialign_1_1;
      semialign-indexed = doJailbreak super.semialign-indexed;
      multistate        = doJailbreak (overrideCabal super.multistate (attrs: { broken = false; }));
      butcher           = doJailbreak (overrideCabal super.butcher (attrs: { broken = false; }));

      brittany = doJailbreak (self.callCabal2nix "brittany"
        (pkgs.fetchFromGitHub {
           owner  = "lspitzner";
           repo   = "brittany";
           rev    = "af227a797d588eda936280dc1c3b0b376735335e";
           sha256 = "0l1nk4dgmlv8vl1d993vnyw3da0kzg4gq8c2zd8sd224f2rz6f35";
           # date = 2019-12-20T01:20:07+01:00;
         }) {});

      ghc-exactprint = dontCheck (self.callCabal2nix "ghc-exactprint"
        (pkgs.fetchFromGitHub {
           owner  = "alanz";
           repo   = "ghc-exactprint";
           rev    = "91f54d7a7a1d8d2131c5e83d13dee6c9e8b57831";
           sha256 = "15yf0ckcb6f706p39w448vgj0nrkd0rk71lvb1nd0ak46y0aqnhb";
           # date = 2019-08-28T20:44:28+02:00;
         }) {});
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
