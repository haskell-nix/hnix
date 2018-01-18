{ nixpkgs ? <nixpkgs>
, system ? builtins.currentSystem
, compiler ? "ghc801"
}:

let
  nixpkgsLocal = import nixpkgs { inherit system; };

  nixpkgsStable = import (nixpkgsLocal.callPackage ./nixpkgs.nix {}) {
    inherit system;
  };
in (import ./default.nix { nixpkgs = nixpkgsStable; inherit compiler; }).env
