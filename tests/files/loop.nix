(let
  requiredVersion = "1.11";
in with builtins;
let
  homeDir = builtins.getEnv "HOME";
in { localSystem ? builtins.intersectAttrs {
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
       inherit (({ lib
          , localSystem
          , crossSystem
          , config
          , overlays
          , bootstrapFiles ? {} }:
         let
         in rec {
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
                     __cur_file = /Users/johnw/src/nix/nixpkgs/pkgs/stdenv/generic/default.nix;
                   in let
                     lib = let
                       __cur_file = /Users/johnw/src/nix/nixpkgs/lib/default.nix;
                     in let
                       inherit (({ ... }:
                         rec {
                           fix = null;
                           fix' = f:
                             let
                               x = f x // {
                                 __unfix__ = null;
                               };
                             in x;
                           extends = null;
                           composeExtensions = null;
                           makeExtensible = makeExtensibleWithCustomName "extend";
                           makeExtensibleWithCustomName = extenderName:
                             rattrs:
                               fix' rattrs // {
                                 ${extenderName} = null;
                               };
                         }) {}) makeExtensible;
                       lib = makeExtensible (self:
                         let
                           callLibs = file:
                             import file { lib = self; };
                         in with self;
                         {
                           trivial = callLibs /Users/johnw/src/nix/nixpkgs/lib/trivial.nix;
                           fixedPoints = null;
                           attrsets = callLibs /Users/johnw/src/nix/nixpkgs/lib/attrsets.nix;
                           lists = callLibs /Users/johnw/src/nix/nixpkgs/lib/lists.nix;
                           strings = callLibs /Users/johnw/src/nix/nixpkgs/lib/strings.nix;
                           stringsWithDeps = null;
                           customisation = callLibs /Users/johnw/src/nix/nixpkgs/lib/customisation.nix;
                           maintainers = null;
                           meta = callLibs /Users/johnw/src/nix/nixpkgs/lib/meta.nix;
                           sources = null;
                           versions = null;
                           modules = null;
                           options = null;
                           types = null;
                           licenses = null;
                           systems = callLibs /Users/johnw/src/nix/nixpkgs/lib/systems;
                           debug = null;
                           generators = null;
                           misc = null;
                           fetchers = null;
                           filesystem = null;
                           platforms = systems.forMeta;
                           inherit (attrsets) attrByPath
                                              hasAttrByPath setAttrByPath
                                              getAttrFromPath attrVals
                                              attrValues catAttrs filterAttrs
                                              filterAttrsRecursive foldAttrs
                                              collect nameValuePair mapAttrs
                                              mapAttrs' mapAttrsToList
                                              mapAttrsRecursive
                                              mapAttrsRecursiveCond genAttrs
                                              isDerivation toDerivation
                                              optionalAttrs zipAttrsWithNames
                                              zipAttrsWith zipAttrs
                                              recursiveUpdateUntil
                                              recursiveUpdate matchAttrs
                                              overrideExisting getOutput
                                              getBin getLib getDev
                                              chooseDevOutputs zipWithNames
                                              zip;
                           inherit (lists) singleton foldr
                                           fold foldl foldl' imap0 imap1
                                           concatMap flatten remove
                                           findSingle findFirst any all
                                           count optional optionals toList
                                           range partition zipListsWith
                                           zipLists reverseList listDfs
                                           toposort sort naturalSort
                                           compareLists take drop sublist
                                           last init crossLists unique
                                           intersectLists subtractLists
                                           mutuallyExclusive;
                           inherit (strings) concatStrings
                                             concatMapStrings
                                             concatImapStrings intersperse
                                             concatStringsSep
                                             concatMapStringsSep
                                             concatImapStringsSep
                                             makeSearchPath
                                             makeSearchPathOutput
                                             makeLibraryPath makeBinPath
                                             makePerlPath optionalString
                                             hasPrefix hasSuffix
                                             stringToCharacters stringAsChars
                                             escape escapeShellArg
                                             escapeShellArgs replaceChars
                                             lowerChars upperChars toLower
                                             toUpper addContextFrom
                                             splitString removePrefix
                                             removeSuffix versionOlder
                                             versionAtLeast getVersion
                                             nameFromURL enableFeature
                                             fixedWidthString
                                             fixedWidthNumber isStorePath
                                             toInt readPathsFromFile
                                             fileContents;
                           inherit (customisation) overrideDerivation
                                                   makeOverridable
                                                   callPackageWith
                                                   callPackagesWith
                                                   extendDerivation hydraJob
                                                   makeScope;
                         });
                     in lib;
                   in lib.makeOverridable ({ name ? null
                                           , preHook ? null
                                           , initialPath
                                           , cc
                                           , shell
                                           , allowedRequisites ? null
                                           , extraAttrs ? null
                                           , overrides ? null
                                           , config
                                           , fetchurlBoot
                                           , setupScript ? ./setup.sh
                                           , extraNativeBuildInputs ? null
                                           , extraBuildInputs ? null
                                           , __stdenvImpureHostDeps ? null
                                           , __extraImpureHostDeps ? null
                                           , stdenvSandboxProfile ? ""
                                           , extraSandboxProfile ? ""
                                           , buildPlatform
                                           , hostPlatform
                                           , targetPlatform }:
                     let
                       defaultNativeBuildInputs = [];
                       defaultBuildInputs = extraBuildInputs;
                       stdenv = derivation (lib.optionalAttrs (allowedRequisites != null) {
                         allowedRequisites = allowedRequisites ++ defaultNativeBuildInputs ++ defaultBuildInputs;
                       } // {
                         inherit name;
                         inherit (buildPlatform) system;
                         builder = shell;
                         args = [ "-e" ./builder.sh ];
                         setup = setupScript;
                         inherit initialPath shell
                                 defaultNativeBuildInputs
                                 defaultBuildInputs;
                       } // lib.optionalAttrs (buildPlatform.isDarwin) {
                         __sandboxProfile = stdenvSandboxProfile;
                         __impureHostDeps = __stdenvImpureHostDeps;
                       }) // rec {
                         meta = {
                           description = null;
                           platforms = null;
                         };
                         inherit buildPlatform
                                 hostPlatform targetPlatform;
                         inherit extraNativeBuildInputs
                                 extraBuildInputs
                                 __extraImpureHostDeps
                                 extraSandboxProfile;
                         inherit (hostPlatform) isDarwin
                                                isLinux isSunOS isHurd isCygwin
                                                isFreeBSD isOpenBSD isi686
                                                isx86_64 is64bit isArm isAarch64
                                                isMips isBigEndian;
                         needsPax = isLinux;
                         inherit (({ lib, config, stdenv }:
                           rec {
                             mkDerivation = { name ? null
                                            , nativeBuildInputs ? []
                                            , propagatedNativeBuildInputs ? []
                                            , buildInputs ? []
                                            , propagatedBuildInputs ? []
                                            , configureFlags ? []
                                            , configurePlatforms ? lib.optionals (stdenv.hostPlatform != stdenv.buildPlatform) null
                                            , doCheck ? null
                                            , doInstallCheck ? null
                                            , crossConfig ? null
                                            , meta ? null
                                            , passthru ? {}
                                            , pos ? null
                                            , separateDebugInfo ? false
                                            , outputs ? [ "out" ]
                                            , __impureHostDeps ? []
                                            , __propagatedImpureHostDeps ? []
                                            , sandboxProfile ? ""
                                            , propagatedSandboxProfile ? ""
                                            , hardeningEnable ? []
                                            , hardeningDisable ? []
                                            , ... }@attrs:
                               let
                               in let
                               in lib.extendDerivation (validity.handled) ({
                                 overrideAttrs = null;
                                 inherit meta passthru;
                               } // passthru) (derivation derivationArg);
                           }) {
                           inherit lib config stdenv;
                         }) mkDerivation;
                         inherit lib;
                         inherit fetchurlBoot;
                         inherit overrides;
                         inherit cc;
                         isCross = targetPlatform != buildPlatform;
                       } // extraAttrs;
                     in stdenv)) {
                     name = "${name}-stdenv-darwin";
                     inherit config shell
                             extraNativeBuildInputs
                             extraBuildInputs;
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
               extraPreHook = "export NIX_CFLAGS_COMPILE+=\" -F/Library/Frameworks\"";
               extraNativeBuildInputs = [];
               extraBuildInputs = [];
               libcxx = null;
               allowedRequisites = [
                 (pkgs.darwin.Libsystem) # THUNK FORCE STARTS HERE
               ];
               overrides = null;
             };
           stagesDarwin = [ ({  }: stage0) stage1 ];
         }) args) stagesDarwin;
     in stagesDarwin }@args:
    let
      configExpr = config;
      crossSystem0 = crossSystem;
    in let
      lib = let
        __cur_file = /Users/johnw/src/nix/nixpkgs/lib/default.nix;
      in let
        inherit (({ ... }:
          rec {
            fix = null;
            fix' = f:
              let
                x = f x // {
                  __unfix__ = null;
                };
              in x;
            extends = null;
            composeExtensions = null;
            makeExtensible = makeExtensibleWithCustomName "extend";
            makeExtensibleWithCustomName = extenderName:
              rattrs:
                fix' rattrs // {
                  ${extenderName} = null;
                };
          }) {}) makeExtensible;
        lib = makeExtensible (self:
          let
            callLibs = file:
              import file { lib = self; };
          in with self;
          {
            trivial = callLibs /Users/johnw/src/nix/nixpkgs/lib/trivial.nix;
            fixedPoints = callLibs /Users/johnw/src/nix/nixpkgs/lib/fixed-points.nix;
            attrsets = callLibs /Users/johnw/src/nix/nixpkgs/lib/attrsets.nix;
            lists = callLibs /Users/johnw/src/nix/nixpkgs/lib/lists.nix;
            strings = callLibs /Users/johnw/src/nix/nixpkgs/lib/strings.nix;
            stringsWithDeps = null;
            customisation = callLibs /Users/johnw/src/nix/nixpkgs/lib/customisation.nix;
            maintainers = null;
            meta = null;
            sources = null;
            versions = null;
            modules = null;
            options = callLibs /Users/johnw/src/nix/nixpkgs/lib/options.nix;
            types = callLibs /Users/johnw/src/nix/nixpkgs/lib/types.nix;
            licenses = null;
            systems = callLibs /Users/johnw/src/nix/nixpkgs/lib/systems;
            debug = null;
            generators = null;
            misc = null;
            fetchers = null;
            filesystem = null;
            platforms = null;
            inherit (trivial) id const
                              concat or and boolToString
                              mergeAttrs flip mapNullable
                              inNixShell min max importJSON
                              warn info nixpkgsVersion mod
                              compare splitByAndCompare
                              functionArgs setFunctionArgs
                              isFunction;
            inherit (fixedPoints) fix fix'
                                  extends composeExtensions
                                  makeExtensible
                                  makeExtensibleWithCustomName;
            inherit (attrsets) attrByPath
                               hasAttrByPath setAttrByPath
                               getAttrFromPath attrVals
                               attrValues catAttrs filterAttrs
                               filterAttrsRecursive foldAttrs
                               collect nameValuePair mapAttrs
                               mapAttrs' mapAttrsToList
                               mapAttrsRecursive
                               mapAttrsRecursiveCond genAttrs
                               isDerivation toDerivation
                               optionalAttrs zipAttrsWithNames
                               zipAttrsWith zipAttrs
                               recursiveUpdateUntil
                               recursiveUpdate matchAttrs
                               overrideExisting getOutput
                               getBin getLib getDev
                               chooseDevOutputs zipWithNames
                               zip;
            inherit (lists) singleton foldr
                            fold foldl foldl' imap0 imap1
                            concatMap flatten remove
                            findSingle findFirst any all
                            count optional optionals toList
                            range partition zipListsWith
                            zipLists reverseList listDfs
                            toposort sort naturalSort
                            compareLists take drop sublist
                            last init crossLists unique
                            intersectLists subtractLists
                            mutuallyExclusive;
            inherit (strings) concatStrings
                              concatMapStrings
                              concatImapStrings intersperse
                              concatStringsSep
                              concatMapStringsSep
                              concatImapStringsSep
                              makeSearchPath
                              makeSearchPathOutput
                              makeLibraryPath makeBinPath
                              makePerlPath optionalString
                              hasPrefix hasSuffix
                              stringToCharacters stringAsChars
                              escape escapeShellArg
                              escapeShellArgs replaceChars
                              lowerChars upperChars toLower
                              toUpper addContextFrom
                              splitString removePrefix
                              removeSuffix versionOlder
                              versionAtLeast getVersion
                              nameFromURL enableFeature
                              fixedWidthString
                              fixedWidthNumber isStorePath
                              toInt readPathsFromFile
                              fileContents;
            inherit (customisation) overrideDerivation
                                    makeOverridable callPackageWith
                                    callPackagesWith
                                    extendDerivation hydraJob
                                    makeScope;
          });
      in lib;
      config = configExpr;
      localSystem = lib.systems.elaborate (builtins.intersectAttrs {
        platform = null;
      } config // args.localSystem);
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
              inherit pkgs;
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
      boot = (stageFuns:
          let
            withAllowCustomOverrides = lib.lists.imap1 (index:
              stageFun:
                prevStage:
                  {
                    allowCustomOverrides = index == 1;
                  } // stageFun prevStage) (lib.lists.reverseList stageFuns);
          in let
            len = builtins.length withAllowCustomOverrides;
            go = pred:
              n:
                if n == len
                  then {}
                  else let
                    cur = let
                      args = builtins.elemAt withAllowCustomOverrides n succ;
                      args' = args // {
                        stdenv = args.stdenv;
                      };
                    in allPackages (args' // {
                      buildPackages = null;
                      targetPackages = null;
                    });
                    succ = go null (n + 1);
                  in cur;
            cur = go null 0;
          in cur);
      stages = stdenvStages {
        inherit lib localSystem crossSystem config overlays;
      };
    in boot stages) (args // {
    inherit config overlays crossSystem;
    localSystem = { system = builtins.currentSystem; };
  })) {}
