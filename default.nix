let
  hostPkgs = import <nixpkgs> {};
  pinnedPkgs = hostPkgs.fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "b0482248fefbf3b6cdd9c92053cfb49778a3a3a8";
    sha256 = "0qdgzdxkdl77381ifmzpikxr2gi5jcmflg9syvbi1h2pjfsi2424";
  };

in { nixpkgs ? import pinnedPkgs {}
   , compiler ? "ghc822"
   , doProfiling ? false
   , doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  haskellPackages = let hpkgs = pkgs.haskell.packages.${compiler}; in
    hpkgs // {
      mkDerivation = args: hpkgs.mkDerivation (args // {
        enableLibraryProfiling = doProfiling;
        enableExecutableProfiling = doProfiling;
      });
    };

  pkg = haskellPackages.developPackage {
    root = ./.;
    source-overrides = {
      # Use a particular commit from github
      insert-ordered-containers = pkgs.fetchFromGitHub {
        owner = "mightybyte";
        repo = "insert-ordered-containers";
        rev = "87054c519b969b62131bcf7a183470d422cbb535";
        sha256 = "0l0g6ns5bcrcaij0wbdgc04qyl9h0vk1kx9lkzdkwj9v51l26azm";
      };
    };
    modifier = drv: pkgs.haskell.lib.overrideCabal drv (attrs: {
      testHaskellDepends = attrs.testHaskellDepends ++
        [ pkgs.nix haskellPackages.hpack ];
    });
  };

  variant = if doBenchmark
            then pkgs.haskell.lib.doBenchmark
            else pkgs.lib.id;

in variant pkg
