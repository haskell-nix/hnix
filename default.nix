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
    rev = "8cc6595803872b7effc4cbf97aec0b8723068212";
    sha256 = "1scm72bxn4wx9r00m0l4h4kanlgq9fw5z1nfzi11d973b5pf1nf3";
  };

  overlay = pkgs.lib.foldr pkgs.lib.composeExtensions (_: _: {}) [
    (import "${hnix-store-src}/overlay.nix")
    (self: super: with pkgs.haskell.lib; {
      mono-traversable = dontCheck super.mono-traversable;
      these = doJailbreak super.these;
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
