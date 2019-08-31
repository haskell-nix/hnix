{ compiler ? "ghc864"

, doBenchmark ? false
, doTracing   ? false
, doOptimize  ? false # enables GHC optimizations for production use
, doProfiling ? false # enables profiling support in GHC
, doStrict    ? false

, withHoogle  ? true

, rev    ? "c4adeddb5f8e945517068968d06ea838b7c24bd3"

, pkgs   ?
    if builtins.compareVersions builtins.nixVersion "2.0" < 0
    then abort "hnix requires at least nix 2.0"
    else import (builtins.fetchGit {
           url = "https://github.com/NixOS/nixpkgs/";
           inherit rev; }) {
           config.allowUnfree = true;
           config.allowBroken = false;
           config.packageOverrides = pkgs: rec {
             nix = pkgs.nixUnstable.overrideDerivation (attrs: {
               src = if builtins.pathExists ./data/nix/version then data/nix else throw "data/nix doesn't seem to contain the nix source. You may want to run git submodule update --init.";
               configureFlags = attrs.configureFlags ++ [ "--disable-doc-gen" ];
               buildInputs = attrs.buildInputs ++
                 [ pkgs.editline.dev
                 ];
               outputs = builtins.filter (s: s != "doc" && s != "man" ) attrs.outputs;
             });
           };
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

      mono-traversable = dontCheck super.mono-traversable;
      these = doJailbreak super.these;
      multistate = doJailbreak (overrideCabal super.multistate (attrs: { broken = false; }));
      butcher = doJailbreak (overrideCabal super.butcher (attrs: { broken = false; }));

      brittany = doJailbreak (self.callCabal2nix "brittany"
        (pkgs.fetchFromGitHub {
           owner  = "lspitzner";
           repo   = "brittany";
           rev    = "6c187da8f8166d595f36d6aaf419370283b3d1e9";
           sha256 = "0nmnxprbwws3w1sh63p80qj09rkrgn9888g7iim5p8611qyhdgky";
           # date = 2018-11-30T22:13:02+01:00;
         }) {});

      ghc-exactprint = dontCheck (self.callCabal2nix "ghc-exactprint"
        (pkgs.fetchFromGitHub {
           owner  = "alanz";
           repo   = "ghc-exactprint";
           rev    = "281f65324fb1fcad8f5ceec06f5ea4c7d78cfb59";
           sha256 = "1d6sjy5mw0jn09sgx7zn0w1gszn3mf6lzqsfv3li50fnvwv1gwzb";
           # date = 2019-03-01T17:38:18+02:00;
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
      haskellPackages.brittany
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
