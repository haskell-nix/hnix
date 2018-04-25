(with builtins;
{ localSystem ? builtins.intersectAttrs {
     system = null;
     platform = null;
   } args
   , system ? null
   , platform ? null
   , crossSystem ? null
   , config ? {}
   , overlays ? []
   , ... }@args:
  ({ localSystem
   , crossSystem ? null
   , config ? null
   , overlays ? null
   , stdenvStages ? { lib, localSystem, crossSystem, config, overlays }@args:
     let
       inherit (
         rec {
           stageFun = step:
             last:
               { shell ? "/bin/bash"
               , overrides ? null
               , allowedRequisites ? null }:
                 let
                   name = "bootstrap-stage${toString step}";
                   thisStdenv = (let
                     fix' = f: let x = f x // { __unfix__ = null; }; in x;
                     makeExtensible = rattrs: fix' rattrs // { extend = null; };
                     lib = makeExtensible (self:
                         let callLibs = file: import file { lib = self; };
                         in with self; {
                           customisation =
                             callLibs /Users/johnw/src/nix/nixpkgs/lib/customisation.nix;
                           inherit (customisation) makeOverridable;
                         });
                   in lib.makeOverridable ({ name ? null
                                           , preHook ? null
                                           , initialPath
                                           , shell
                                           , allowedRequisites ? null
                                           , overrides ? null
                                           , config
                                           , buildPlatform
                                           , hostPlatform
                                           , targetPlatform }:
                     let
                       defaultBuildInputs = [];
                       stdenv = derivation
                         ({ allowedRequisites =
                              allowedRequisites ++
                              defaultBuildInputs;
                          } // {
                         inherit name;
                         inherit (buildPlatform) system;
                         builder = shell;
                       }) // {
                         inherit overrides;
                       };
                     in stdenv)) {
                     name = "${name}-stdenv-darwin";
                     inherit config shell;
                     allowedRequisites = if allowedRequisites == null
                       then null else allowedRequisites ++ [];
                     buildPlatform = localSystem;
                     hostPlatform = localSystem;
                     targetPlatform = localSystem;
                     initialPath = [];
                     overrides = self: super:
                         overrides null super // { fetchurl = null; };
                   };
                 in {
                   inherit config overlays;
                   stdenv = thisStdenv;
                 };
           stage0 = stageFun 0 null {
             overrides = self: super: super;
           };
           stage1 = prevStage:
             with prevStage;
             stageFun 1 prevStage {
               allowedRequisites = [
                 (pkgs.darwin.Libsystem) # THUNK FORCE STARTS HERE
               ];
               overrides = null;
             };
           stagesDarwin = [ ({  }: stage0) stage1 ];
         }) stagesDarwin;
     in stagesDarwin }@args:
    let
      lib = let
        fix' = f: let x = f x // { __unfix__ = null; }; in x;
        makeExtensible = rattrs: fix' rattrs // { extend = null; };
        in makeExtensible (self:
            let callLibs = file: import file { lib = self; };
            in with self;
            {
              fixedPoints = callLibs /Users/johnw/src/nix/nixpkgs/lib/fixed-points.nix;
              lists = callLibs /Users/johnw/src/nix/nixpkgs/lib/lists.nix;
              inherit (fixedPoints) fix extends;
              inherit (lists) foldl' imap1;
            });

      allPackages = newArgs:
        ({ lib
         , nixpkgsFun
         , stdenv
         , allowCustomOverrides
         , noSysDirs ? null
         , config
         , overlays }:
          let
            allPackages = pkgs: super: {
              pkgs = pkgs;
              darwin = pkgs.darwin; # THUNK FORCE LOOPS: self-reference
            };
          in lib.fix (lib.foldl' (x: y: lib.extends y x) (self:
              {}) ([
              (self: super: { inherit stdenv; })
              allPackages
              (self: super: super.stdenv.overrides null super)
            ]))) ({ inherit lib nixpkgsFun; } // newArgs);

       withAllowCustomOverrides = lib.lists.imap1 (index: stageFun: prevStage:
           { allowCustomOverrides = index == 1; } // stageFun prevStage)
         (lib.lists.reverseList
          (stdenvStages {
             inherit lib localSystem crossSystem config overlays;
           }));

       go = n:
         if n == builtins.length withAllowCustomOverrides
         then {}
         else let
           succ = go (n + 1);
           in allPackages (builtins.elemAt withAllowCustomOverrides n succ);
     in go 0)

  (args // {
     inherit config overlays crossSystem;
     localSystem = { system = builtins.currentSystem; };
   })) {}
