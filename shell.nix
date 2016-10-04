{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc801" }:
(import ./default.nix { inherit nixpkgs compiler; }).env
