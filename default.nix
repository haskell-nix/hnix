{ nixpkgs ? import <nixpkgs> {}, compiler ? null }:
let
  haskellPackages = if compiler == null
                    # use the current default version
                    then nixpkgs.pkgs.haskellPackages
                    else nixpkgs.pkgs.haskell.packages.${compiler};
in

haskellPackages.callPackage ./project.nix {
  pkgs = nixpkgs;
}
