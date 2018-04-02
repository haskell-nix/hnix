{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, ansi-wl-pprint, base, containers, criterion
      , data-fix, deepseq, deriving-compat, directory, filepath, Glob
      , parsers, regex-tdfa, regex-tdfa-text, semigroups, split, stdenv
      , tasty, tasty-hunit, tasty-th, text, transformers, trifecta
      , unordered-containers, these, optparse-applicative, interpolate
      , process, exceptions, bytestring, mtl, monadlist
      }:
      mkDerivation {
        pname = "hnix";
        version = "0.4.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          ansi-wl-pprint base containers data-fix deepseq deriving-compat
          parsers regex-tdfa regex-tdfa-text semigroups text transformers
          trifecta unordered-containers these process directory filepath
          exceptions bytestring mtl monadlist
        ];
        executableHaskellDepends = [
          ansi-wl-pprint base containers data-fix deepseq optparse-applicative
          text transformers
        ];
        testHaskellDepends = [
          base containers data-fix directory filepath Glob split tasty
          tasty-hunit tasty-th text transformers interpolate
        ];
        benchmarkHaskellDepends = [ base containers criterion text ];
        homepage = "http://github.com/jwiegley/hnix";
        description = "Haskell implementation of the Nix language";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
