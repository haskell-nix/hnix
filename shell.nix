{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc7103" }:
(import ./default.nix { inherit nixpkgs compiler; }).env
