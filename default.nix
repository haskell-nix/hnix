{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc7102" }:
let
  haskellPackages = nixpkgs.pkgs.haskell.packages.${compiler};
in

haskellPackages.callPackage ./project.nix {}
