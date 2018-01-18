{ nixpkgs ? import <nixpkgs> {}, compiler ? null }:
let
  haskellPackages =
    if compiler == null || !(nixpkgs.lib.hasAttr compiler nixpkgs.pkgs.haskell.packages)
      # use the current default version
      then nixpkgs.pkgs.haskellPackages
      else nixpkgs.lib.attrByPath [compiler] {} nixpkgs.pkgs.haskell.packages;
in

haskellPackages.callPackage ./project.nix {
  pkgs = nixpkgs;
}
