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
                    hlint = {};
                    haskell-language-server = {};
                    fourmolu = {};
                  };
                  buildInputs = with pkgs; [
                    pkg-config
                    shellcheck
                    shfmt
                    lefthook
                  ];
                  withHoogle = false;
                };
                modules = [{
                  packages.hnix.doCheck = false;
                  # enableLibraryProfiling = true;
                  # enableProfiling = true;
                }];
              };
          })
        ];
      in flake // {
        packages.default = flake.packages."hnix:exe:hnix";

        checks = (flake.checks or {}) // {
          formatting = pkgs.runCommand "check-formatting" {
            nativeBuildInputs = [
              pkgs.haskellPackages.fourmolu
              pkgs.findutils
            ];
          } ''
            cd ${./.}
            find src main tests benchmarks -name '*.hs' \
              -exec fourmolu --mode check {} +
            touch $out
          '';

          hlint = pkgs.runCommand "check-hlint" {
            nativeBuildInputs = [ pkgs.hlint ];
          } ''
            cd ${./.}
            hlint src/ main/ tests/ benchmarks/
            touch $out
          '';

          shellcheck = pkgs.runCommand "check-shellcheck" {
            nativeBuildInputs = [ pkgs.shellcheck ];
          } ''
            cd ${./.}
            shellcheck build.sh
            touch $out
          '';

          shell-formatting = pkgs.runCommand "check-shell-formatting" {
            nativeBuildInputs = [ pkgs.shfmt ];
          } ''
            cd ${./.}
            shfmt -d build.sh
            touch $out
          '';
        };
      });
}

