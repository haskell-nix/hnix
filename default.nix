{ compiler ? "ghc863"

, doBenchmark ? false
, doTracing   ? false
, doOptimize  ? false # enables GHC optimizations for production use
, doProfiling ? false # enables profiling support in GHC
, doStrict    ? false

, withHoogle  ? false

, rev    ? "120eab94e0981758a1c928ff81229cd802053158"
, sha256 ? "0qk6k8gxx5xlkyg05dljywj5wx5fvrc3dzp4v2h6ab83b7zwg813"

, pkgs   ?
    if builtins.compareVersions builtins.nixVersion "2.0" < 0
    then abort "hnix requires at least nix 2.0"
    else import (builtins.fetchTarball {
           url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
           inherit sha256; }) {
           config.allowUnfree = true;
           config.allowBroken = false;
           config.packageOverrides = pkgs: rec {
             nix = pkgs.nixUnstable.overrideDerivation (attrs: {
               src = data/nix;
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
      mono-traversable = dontCheck super.mono-traversable;
      these = doJailbreak super.these;
      multistate = doJailbreak super.multistate;

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

drv = haskellPackages.developPackage {
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
};

in drv
