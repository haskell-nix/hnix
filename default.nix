let
  hostPkgs = import <nixpkgs> {};
  pinnedPkgs = hostPkgs.fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs-channels";
    rev = "ee28e35ba37ab285fc29e4a09f26235ffe4123e2";
    sha256 = "0a6xrqjj2ihkz1bizhy5r843n38xgimzw5s2mfc42kk2rgc95gw5";
  };

in { nixpkgs ? import pinnedPkgs {}
   , compiler ? "ghc822"
   , doProfiling ? false
   , doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  haskellPackages = pkgs.haskell.packages.${compiler};

  pkg = haskellPackages.developPackage {
    root = ./.;
    source-overrides = {
      # Use a particular commit from github
      insert-ordered-containers = pkgs.fetchFromGitHub {
        owner = "mightybyte";
        repo = "insert-ordered-containers";
        rev = "2a15aea6a9733259ee494eb379dd4df206d215c5";
        sha256 = "1pjg9lwahm767mf88r6cb0dcaga84l8p08zd7mxjz322ll07q1ja";
      };
    };
  };

  variant =
    if doBenchmark
    then pkgs.haskell.lib.doBenchmark
    else if doProfiling
         then pkgs.haskell.lib.doProfiling
         else pkgs.lib.id;

in variant pkg
