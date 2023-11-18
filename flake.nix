{
  description = "A Haskell re-implementation of the Nix expression language";

  inputs = {
    nixpkgs.url = "nixpkgs/8e5e424b1c059e9ccf5db6a652458e30de05fa3a";
    nix = {
      url = "nix/624e38aa43f304fbb78b4779172809add042b513";
      flake = false;
    };
  };

  outputs = {
    nix,
    nixpkgs,
    self,
  } @ inp: let

    l = builtins //nixpkgs.lib;
    supportedSystems = ["x86_64-linux" "aarch64-darwin"];

    forAllSystems = f: l.genAttrs supportedSystems
      (system: f system (nixpkgs.legacyPackages.${system}));

  in {

    defaultPackage = forAllSystems
      (system: pkgs: import ./default.nix {
        inherit pkgs;
        withHoogle = true;
        compiler = "ghc947";
        packageRoot = pkgs.runCommand "hnix-src" {} ''
          cp -r ${./.} $out
          chmod -R +w $out
          cp -r ${nix} $out/data/nix
        '';
      });

    devShell = forAllSystems (system: pkgs: self.defaultPackage.${system}.env);
  };
}
