{
  description = "A Haskell re-implementation of the Nix expression language";

  inputs = {
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    nix.url = "github:NixOS/nix";
    haskellNix.url = "github:input-output-hk/haskell.nix/pull/2434/merge";
    flake-utils.url = "github:numtide/flake-utils";
  };

  # Binary Cache for haskell.nix
  nixConfig = {
    extra-trusted-public-keys = [ "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" ];
    extra-substituters = [ "https://cache.iog.io" ];
  };

  outputs = { self, nixpkgs, nix, flake-utils, haskellNix }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system overlays;
          inherit (haskellNix) config;
        };
        flake = pkgs.hnix.flake {
        };
        overlays = [ haskellNix.overlay
          (final: prev: {
            hnix =
              final.haskell-nix.project' {
                src = ./.;
                supportHpack = true;
                compiler-nix-name = "ghc910";
                # packageRoot = pkgs.runCommand "hnix-src" {} ''
                #   cp -r ${./.} $out
                #   chmod -R +w $out
                #   cp -r ${nix} $out/data/nix
                # '';
                shell = {
                  tools = {
                    cabal = {};
                    hlint = {};
                    haskell-language-server = {};
                  };
                  buildInputs = with pkgs; [
                    pkg-config
                  ];
                  withHoogle = false;
                };
                # modules = [{
                #   # enableLibraryProfiling = true;
                #   # enableProfiling = true;
                #   # profilingDetail = "late-toplevel";
                #   # TODO(@connorbaker): Can't build XML with overloaded-calls.
                #   # ghcOptions = [
                #   #   "-fprof-late-overloaded"
                #   # ];
                # }];
              };
          })
        ];
      in flake // {
        legacyPackages = pkgs;
        packages.default = flake.packages."hnix:exe:hnix";
      });
}

