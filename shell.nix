{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc7102" }:
(import ./default.nix { inherit nixpkgs compiler; }).env
