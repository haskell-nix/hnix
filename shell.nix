{ pkgs ? import <nixpkgs> {} }: pkgs.haskellPackages.callPackage ./default.nix {}
