#!/usr/bin/env bash

# NOTE: Script for the CI builds, relies on `default.nix` interface, which exposes Nixpkgs Haskell Lib interface

# The most strict error checking requirements
set -Eexuo pipefail

### NOTE: Section handles imports from env, these are settings for Nixpkgs.
# Some of these options implicitly switch the dependent options.


# NOTE: If var not imported - set to the default value
GHCVERSION=${GHCVERSION:-'ghc8101'}
# NOTE: Nix by default uses nixpkgs-unstable channel
# Setup for Nixpkgs revision:
#   `rev` vals in order of freshness -> cache & stability:
#   { master
#   , commitHash
#   , haskell-updates  # Haskell development branch in Nixpkgs, can be inconsistent. Weekly merged into the upstream
#   , nixpkgs-unstable  # Default branch on Nix installation, default for non NixOS
#   , nixos-unstable  # nixpkgs-unstable that passes a bunch of base tests
#   , nixos-20.03  # Last stable release, gets almost no updates to recipes, gets only required backports
#   ...
#   }
rev=${rev:-'nixpkgs-unstable'}
# If NIX_PATH not imported - construct it from `rev`
NIX_PATH=${NIX_PATH:-"nixpkgs=https://github.com/nixos/nixpkgs/archive/$rev.tar.gz"}
export NIX_PATH
# NOTE: Project name, used by cachix
project=${project:-'defaultProjectName'}

# This settings expose most of the Nixpkgs Haskell.lib API: https://github.com/NixOS/nixpkgs/blob/master/pkgs/development/haskell-modules/lib.nix

# Don't fail at configure time if there are multiple versions of the same package in the (recursive) dependencies of the package being built. Will delay failures, if any, to compile time.
allowInconsistentDependencies=${allowInconsistentDependencies:-'false'}

# Escape the version bounds from the cabal file. You may want to avoid this function.
doJailbreak=${doJailbreak:-'false'}
# Nix dependency checking, compilation and execution of test suites listed in the package description file.
doCheck=${doCheck:-'true'}
# Just produce a SDist src tarball
sdistTarball=${sdistTarball:-'false'}
# Produce SDist tarball and build project from it
# The strict packaging process as used on Hackage. Tests consistency of the Cabal file.
buildFromSdist=${buildFromSdist:-'false'}
# Turn all warn into err with {-Wall,-Werror}
failOnAllWarnings=${failOnAllWarnings:-'false'}
# `failOnAllWarnings` + `buildFromSdist`
buildStrictly=${buildStrictly:-'false'}
#  2020-06-02: NOTE: enableDeadCodeElimination = true: On GHC =< 8.8.3 macOS build falls due to https://gitlab.haskell.org/ghc/ghc/issues/17283
enableDeadCodeElimination=${enableDeadCodeElimination:-'false'}
# Disabled GHC code optimizations make build/tolling/dev loops faster. Works for Haskel IDE Engine and GHCID
# Enable optimizations for production use, and to pass benchmarks.
disableOptimization=${disableOptimization:-'true'}
# Use faster `gold` ELF linker from GNU binutils instead of older&slower but more versatile GNU linker. Is not available by default since macOS does not have it.
linkWithGold=${linkWithGold:-'false'}
# Provide an inventory of performance events and timings for the execution. Provides informaiton in an absolute sense. Nothing is timestamped.
enableLibraryProfiling=${enableLibraryProfiling:-'false'}
enableExecutableProfiling=${enableExecutableProfiling:-'false'}
# Include tracing information & abilities. Tracing records the chronology, often with timestamps and is extensive in time
doTracing=${doTracing:-'false'}
# Include DWARF debugging information & abilities
enableDWARFDebugging=${enableDWARFDebugging:-'false'}
# Strip results from all debugging symbols
doStrip=${doStrip:-'false'}
#	Generate hyperlinked source code for documentation using HsColour, and have Haddock documentation link to it.
doHyperlinkSource=${doHyperlinkSource:-'false'}
# Nixpkgs expects shared libraries
enableSharedLibraries=${enableSharedLibraries:-'true'}
# Ability to make static libraries
enableStaticLibraries=${enableStaticLibraries:-'false'}
# Make hybrid executable that is also a shared library
enableSharedExecutables=${enableSharedExecutables:-'false'}
# link executables statically against haskell libs to reduce closure size
justStaticExecutables=${justStaticExecutables:-'false'}
enableSeparateBinOutput=${enableSeparateBinOutput:-'false'}
# Add a post-build check to verify that dependencies declared in the .cabal file are actually used.
# checkUnusedPackages: is `failOnAllWarnings` + `cabal sdist` to ensure all needed files are listed in the Cabal file. Currently uses `packunused` or GHC 8.8 internals, later switches into GHC internal feature. Adds a post-build check to verify that dependencies declared in the cabal file are actually used.
checkUnusedPackages=${checkUnusedPackages:-'false'}
# Generation and installation of haddock API documentation
doHaddock=${doHaddock:-'false'}
# Generation and installation of a coverage report. See https://wiki.haskell.org/Haskell_program_coverage
doCoverage=${doCoverage:-'false'}
# doBenchmark: Dependency checking + compilation and execution for benchmarks listed in the package description file.
doBenchmark=${doBenchmark:-'false'}
# For binaries named in `executableNamesToShellComplete` list, generate and bundle-into package an automatically loaded shell complettions
generateOptparseApplicativeCompletions=${generateOptparseApplicativeCompletions:-'false'}
# [ "binary1" "binary2" ] - should pass " quotes into Nix interpreter
executableNamesToShellComplete=${executableNamesToShellComplete:-'[ "defaultBinaryName" ]'}

# Include Hoogle into derivation
withHoogle=${withHoogle:-'false'}

# Log file to dump GHCJS build into
ghcjsTmpLogFile=${ghcjsTmpLogFile:-'/tmp/ghcjsTmpLogFile.log'}
# Length of the GHCJS log tail (<40000)
ghcjsLogTailLength=${ghcjsLogTailLength:-'10000'}

# If key not provided (branch is not inside the central repo) - init CACHIX_SIGNING_KEY as empty
CACHIX_SIGNING_KEY=${CACHIX_SIGNING_KEY:-""}


GHCJS_BUILD(){
# NOTE: Function for GHCJS build that outputs its huge log into a file

  # NOTE: Run the build into Log (log is too long for Travis)
  "$@" &> "$ghcjsTmpLogFile"

}

SILENT(){
# NOTE: Function that silences the build process
# In normal mode outputs only the /nix/store paths

  echo "Log: $ghcjsTmpLogFile"
  # NOTE: Pass into the ghcjsbuild function the build command
  if GHCJS_BUILD "$@"
  then

    # NOTE: Output log lines for stdout -> cachix caching
    grep '^/nix/store/' "$ghcjsTmpLogFile"

  else

    # NOTE: Output log lines for stdout -> cachix caching
    grep '^/nix/store/' "$ghcjsTmpLogFile"

    # NOTE: Propagate the error state, fail the CI build
    exit 1

  fi

}

BUILD_PROJECT(){


IFS=$'\n\t'

if [ "$GHCVERSION" = "ghcjs" ]
  then

    # NOTE: GHCJS build
    # By itself, GHCJS creates >65000 lines of log that are >4MB in size, so Travis terminates due to log size quota.
    # nixbuild --quiet (x5) does not work on GHC JS build
    # So there was a need to make it build.
    # The solution is to silence the stdout
    # But Travis then terminates on 10 min no stdout timeout
    # so HACK: SILENT wrapper allows to surpress the huge log, while still preserves the Cachix caching ability in any case of the build
    # On build failure outputs the last 10000 lines of log (that should be more then enough), and terminates
    SILENT nix-build \
      --arg allowInconsistentDependencies "$allowInconsistentDependencies" \
      --arg doJailbreak "$doJailbreak" \
      --arg doCheck "$doCheck" \
      --arg sdistTarball "$sdistTarball" \
      --arg buildFromSdist "$buildFromSdist" \
      --arg failOnAllWarnings "$failOnAllWarnings" \
      --arg buildStrictly "$buildStrictly" \
      --arg enableDeadCodeElimination "$enableDeadCodeElimination" \
      --arg disableOptimization "$disableOptimization" \
      --arg linkWithGold "$linkWithGold" \
      --arg enableLibraryProfiling "$enableLibraryProfiling" \
      --arg enableExecutableProfiling "$enableExecutableProfiling" \
      --arg doTracing "$doTracing" \
      --arg enableDWARFDebugging "$enableDWARFDebugging" \
      --arg doStrip "$doStrip" \
      --arg doHyperlinkSource "$doHyperlinkSource" \
      --arg enableSharedLibraries "$enableSharedLibraries" \
      --arg enableStaticLibraries "$enableStaticLibraries" \
      --arg enableSharedExecutables "$enableSharedExecutables" \
      --arg justStaticExecutables "$justStaticExecutables" \
      --arg checkUnusedPackages "$checkUnusedPackages" \
      --arg doCoverage "$doCoverage" \
      --arg doHaddock "$doHaddock" \
      --arg doBenchmark "$doBenchmark" \
      --arg generateOptparseApplicativeCompletions "$generateOptparseApplicativeCompletions" \
      --arg executableNamesToShellComplete "$executableNamesToShellComplete" \
      --arg withHoogle "$withHoogle" \
      ghcjs

  else

    # NOTE: Normal GHC build
    # NOTE: GHC sometimes produces logs so big - that Travis terminates builds, so multiple --quiet
    nix-build \
      --quiet --quiet \
      --argstr compiler "$GHCVERSION" \
      --arg allowInconsistentDependencies "$allowInconsistentDependencies" \
      --arg doJailbreak "$doJailbreak" \
      --arg doCheck "$doCheck" \
      --arg sdistTarball "$sdistTarball" \
      --arg buildFromSdist "$buildFromSdist" \
      --arg failOnAllWarnings "$failOnAllWarnings" \
      --arg buildStrictly "$buildStrictly" \
      --arg enableDeadCodeElimination "$enableDeadCodeElimination" \
      --arg disableOptimization "$disableOptimization" \
      --arg linkWithGold "$linkWithGold" \
      --arg enableLibraryProfiling "$enableLibraryProfiling" \
      --arg enableExecutableProfiling "$enableExecutableProfiling" \
      --arg doTracing "$doTracing" \
      --arg enableDWARFDebugging "$enableDWARFDebugging" \
      --arg doStrip "$doStrip" \
      --arg doHyperlinkSource "$doHyperlinkSource" \
      --arg enableSharedLibraries "$enableSharedLibraries" \
      --arg enableStaticLibraries "$enableStaticLibraries" \
      --arg enableSharedExecutables "$enableSharedExecutables" \
      --arg justStaticExecutables "$justStaticExecutables" \
      --arg checkUnusedPackages "$checkUnusedPackages" \
      --arg doCoverage "$doCoverage" \
      --arg doHaddock "$doHaddock" \
      --arg doBenchmark "$doBenchmark" \
      --arg generateOptparseApplicativeCompletions "$generateOptparseApplicativeCompletions" \
      --arg executableNamesToShellComplete "$executableNamesToShellComplete" \
      --arg withHoogle "$withHoogle"

fi
}

MAIN() {


#  2020-06-01: NOTE: Nix installer installs old Nix version that has bugs that prevented importing Nixpks repository channels, updating to latest Nix since it does not have that bug
# NOTE: Overall it is useful to have in CI test builds the latest stable Nix
# NOTE: User-run update for Linux setup
nix upgrade-nix || true
# NOTE: Superuser update for macOS setup
sudo nix upgrade-nix || true

# NOTE: Make channels current
nix-channel --update || true
sudo nix-channel --update || true


# NOTE: Secrets are not shared to PRs from forks
# NOTE: nix-build | cachix push <project> - uploads binaries, runs&works only in the branches of the main repository, so for PRs - else case runs

  if [ ! "$CACHIX_SIGNING_KEY" = "" ]

    then

      # NOTE: Build of the inside repo branch - enable push Cachix cache
      BUILD_PROJECT | cachix push "$project"

    else

      # NOTE: Build of the side repo/PR - can not push Cachix cache
      BUILD_PROJECT

  fi

}

# NOTE: Run the entry function of the script
MAIN
