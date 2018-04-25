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
   , stdenvStages ? { lib
                    , localSystem
                    , crossSystem
                    , config
                    , overlays }@args:
     let
       inherit (
         rec {
           stageFun = step:
             last:
               { shell ? "/bin/bash"
               , overrides ? null
               , extraPreHook ? ""
               , extraNativeBuildInputs
               , extraBuildInputs
               , libcxx
               , allowedRequisites ? null }:
                 let
                   name = "bootstrap-stage${toString step}";
                   cc = "/dev/null";
                   thisStdenv = (let
                     lib = let
                       inherit (rec {
                           fix' = f:
                             let
                               x = f x // {
                                 __unfix__ = null;
                               };
                             in x;
                           makeExtensible =
                             rattrs:
                               fix' rattrs // {
                                 extend = null;
                               };
                         }) makeExtensible;
                       lib = makeExtensible (self:
                         let
                           callLibs = file:
                             import file { lib = self; };
                         in with self;
                         {
                           attrsets = callLibs /Users/johnw/src/nix/nixpkgs/lib/attrsets.nix;
                           customisation = callLibs /Users/johnw/src/nix/nixpkgs/lib/customisation.nix;
                           inherit (attrsets) optionalAttrs;
                           inherit (customisation) makeOverridable;
                         });
                     in lib;
                   in lib.makeOverridable ({ name ? null
                                           , preHook ? null
                                           , initialPath
                                           , cc
                                           , shell
                                           , allowedRequisites ? null
                                           , overrides ? null
                                           , config
                                           , fetchurlBoot
                                           , buildPlatform
                                           , hostPlatform
                                           , targetPlatform }:
                     let
                       defaultNativeBuildInputs = [];
                       defaultBuildInputs = extraBuildInputs;
                       stdenv = derivation
                         (lib.optionalAttrs (allowedRequisites != null) {
                         allowedRequisites =
                           allowedRequisites ++
                           defaultNativeBuildInputs ++
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
                       then null
                       else allowedRequisites ++ [];
                     buildPlatform = localSystem;
                     hostPlatform = localSystem;
                     targetPlatform = localSystem;
                     inherit cc;
                     initialPath = [
                     ];
                     fetchurlBoot = null;
                     overrides = self:
                       super:
                         overrides null super // {
                           fetchurl = null;
                         };
                   };
                 in {
                   inherit config overlays;
                   stdenv = thisStdenv;
                 };
           stage0 = stageFun 0 null {
             overrides = self: super: super;
             extraNativeBuildInputs = [];
             extraBuildInputs = [];
             libcxx = null;
           };
           stage1 = prevStage:
             with prevStage;
             stageFun 1 prevStage {
               extraNativeBuildInputs = [];
               extraBuildInputs = [];
               libcxx = null;
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
        inherit (
          rec {
            fix' = f:
              let
                x = f x // {
                  __unfix__ = null;
                };
              in x;
            makeExtensible = rattrs: fix' rattrs // { extend = null; };
          }) makeExtensible;
        lib = makeExtensible (self:
          let callLibs = file: import file { lib = self; };
          in with self;
          {
            trivial = callLibs /Users/johnw/src/nix/nixpkgs/lib/trivial.nix;
            fixedPoints = callLibs /Users/johnw/src/nix/nixpkgs/lib/fixed-points.nix;
            lists = callLibs /Users/johnw/src/nix/nixpkgs/lib/lists.nix;
            inherit (trivial) flip;
            inherit (fixedPoints) fix extends;
            inherit (lists) foldl' imap1;
          });
      in lib;

      allPackages = newArgs:
        ({ lib
         , nixpkgsFun
         , buildPackages
         , targetPackages
         , stdenv
         , allowCustomOverrides
         , noSysDirs ? null
         , config
         , overlays }:
          let
            stdenvBootstappingAndPlatforms = self: super: { inherit stdenv; };
            allPackages = pkgs: super: {
              pkgs = pkgs;
              darwin = pkgs.darwin; # THUNK FORCE LOOPS: self-reference
              fetchzip = null;
              fetchurlBoot = null;
              libcxx = null;
              libcxxabi = null;
              xorg = null;
            };
            stdenvOverrides = self:
              super:
                super.stdenv.overrides null super;
            toFix = lib.foldl' (lib.flip (lib.extends)) (self:
              {}) ([
              stdenvBootstappingAndPlatforms
              allPackages
              stdenvOverrides
            ]);
          in lib.fix toFix) ({
          inherit lib nixpkgsFun;
        } // newArgs);

       withAllowCustomOverrides = lib.lists.imap1 (index:
             stageFun:
               prevStage:
                 {
                   allowCustomOverrides = index == 1;
                 } // stageFun prevStage)
           (lib.lists.reverseList
            (stdenvStages {
               inherit lib localSystem crossSystem config overlays;
             }));

       go = n:
         if n == builtins.length withAllowCustomOverrides
         then {}
         else let
           succ = go (n + 1);
           in allPackages (builtins.elemAt withAllowCustomOverrides n succ // {
               buildPackages = null;
               targetPackages = null;
             });
     in go 0)

  (args // {
     inherit config overlays crossSystem;
     localSystem = { system = builtins.currentSystem; };
   })) {}
