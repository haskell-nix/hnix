{
  description = "A Haskell re-implementation of the Nix expression language";

  inputs = {
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    nix.url = "github:NixOS/nix";
    haskellNix.url = "github:input-output-hk/haskell.nix";
    flake-utils.url = "github:numtide/flake-utils";
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
                    haskell-language-server = {};
                  };
                  buildInputs = with pkgs; [
                    pkg-config
                  ];
                  withHoogle = false;
                };
                # modules = [{
                #   enableLibraryProfiling = true;
                #   enableProfiling = true;
                # }];
              };
          })
        ];
      in flake // {
        packages.default = flake.packages."hnix:exe:hnix";
      });
}

