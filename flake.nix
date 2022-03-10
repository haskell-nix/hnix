{
  description = "A Haskell re-implementation of the Nix expression language";

  inputs = {
    nixpkgs.url = "nixpkgs";
    nix = {
      url = "nix/61e816217bfdfffd39c130c7cd24f07e640098fc";
      flake = false;
    };
  };

  outputs = {
    nix,
    nixpkgs,
    self,
  } @ inp: let

    l = builtins //nixpkgs.lib;
    supportedSystems = ["x86_64-linux"];

    forAllSystems = f: l.genAttrs supportedSystems
      (system: f system (nixpkgs.legacyPackages.${system}));

  in {

    defaultPackage = forAllSystems
      (system: pkgs: import ./default.nix {
        inherit pkgs;
        withHoogle = true;
        compiler = "ghc8107";
        packageRoot = pkgs.runCommand "hnix-src" {} ''
          cp -r ${./.} $out
          chmod -R +w $out
          cp -r ${nix} $out/data/nix
        '';
      });

    devShell = forAllSystems (system: pkgs: self.defaultPackage.${system}.env);
  };
}
