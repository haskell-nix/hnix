{
  description = "A Haskell re-implementation of the Nix expression language";

  inputs = {
    nixpkgs.url = "nixpkgs/c757e9bd77b16ca2e03c89bf8bc9ecb28e0c06ad";
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
