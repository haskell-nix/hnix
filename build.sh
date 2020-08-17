#!/usr/bin/env bash

# NOTE: Script to easy import nix-build settings from env, useful for tooling env replication and the CI builds, relies on `default.nix` interface, which exposes Nixpkgs Haskell Lib interface

# The most strict error checking requirements
set -Eexuo pipefail

### NOTE: Section handles imports from env, these are settings for Nixpkgs.
### They use the `default.nix` interface, which exposes expose most of the Nixpkgs Haskell.lib API: https://github.com/NixOS/nixpkgs/blob/master/pkgs/development/haskell-modules/lib.nix
### Some of these options implicitly switch the dependent options.
### Documentation of this settings is mosly in `default.nix`, since most settings it Nixpkgs related
### Additional documentation is in Nixpkgs Haskell.lib: https://github.com/NixOS/nixpkgs/blob/master/pkgs/development/haskell-modules/lib.nix


# NOTE: If vars not imported - init the vars with default values
# Non-set/passed ''/'default' compiler setting means currently default GHC of Nixpkgs ecosystem.
compiler=${compiler:-'default'}

packageRoot=${packageRoot:-'pkgs.nix-gitignore.gitignoreSource [ ] ./.'}
cabalName=${cabalName:-'hnix'}
rev=${rev:-'default'}

# Account in Cachix to use
cachixAccount=${cachixAccount:-'hnix'}

allowInconsistentDependencies=${allowInconsistentDependencies:-'false'}
doJailbreak=${doJailbreak:-'false'}
doCheck=${doCheck:-'true'}

sdistTarball=${sdistTarball:-'false'}
buildFromSdist=${buildFromSdist:-'false'}

failOnAllWarnings=${failOnAllWarnings:-'false'}
buildStrictly=${buildStrictly:-'false'}

enableDeadCodeElimination=${enableDeadCodeElimination:-'false'}
disableOptimization=${disableOptimization:-'true'}
linkWithGold=${linkWithGold:-'false'}

enableLibraryProfiling=${enableLibraryProfiling:-'false'}
enableExecutableProfiling=${enableExecutableProfiling:-'false'}
doTracing=${doTracing:-'false'}
enableDWARFDebugging=${enableDWARFDebugging:-'false'}
doStrip=${doStrip:-'false'}

enableSharedLibraries=${enableSharedLibraries:-'true'}
enableStaticLibraries=${enableStaticLibraries:-'false'}
enableSharedExecutables=${enableSharedExecutables:-'false'}
justStaticExecutables=${justStaticExecutables:-'false'}
enableSeparateBinOutput=${enableSeparateBinOutput:-'false'}

checkUnusedPackages=${checkUnusedPackages:-'false'}
doHaddock=${doHaddock:-'false'}
doHyperlinkSource=${doHyperlinkSource:-'false'}
doCoverage=${doCoverage:-'false'}
doBenchmark=${doBenchmark:-'false'}
generateOptparseApplicativeCompletions=${generateOptparseApplicativeCompletions:-'false'}
# [ "binary1" "binary2" ] - should pass " quotes into Nix interpreter
executableNamesToShellComplete=${executableNamesToShellComplete:-'[ "hnix" ]'}


withHoogle=${withHoogle:-'false'}


# If key not provided (branch is not inside the central repo) - init CACHIX_SIGNING_KEY as empty
CACHIX_SIGNING_KEY=${CACHIX_SIGNING_KEY:-""}

BUILD_PROJECT(){


IFS=$'\n\t'

if [ ! "$compiler" = "ghcjs" ]
   then

    # Normal GHC build
    nix-build \
      --argstr compiler "$compiler" \
      --argstr rev "$rev" \
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


# Overall it is useful to have in CI test builds the latest stable Nix
# 2020-06-24: HACK: Do not ask why different commands on Linux and macOS. IDK, wished they we the same. These are the only commands that worked on according platforms right after the fresh Nix installer rollout.
# 2020-07-06: HACK: GitHub Actions CI shown that nix-channel or nix-upgrade-nix do not work, there is probably some new rollout, shortcircuting for the time bing with || true
(nix-channel --update && nix-env -u) || (sudo nix upgrade-nix) || true


# Report the Nixpkgs channel revision
nix-instantiate --eval -E 'with import <nixpkgs> {}; lib.version or lib.nixpkgsVersion'


# Secrets are not shared to PRs from forks
# nix-build | cachix push <account> - uploads binaries, runs&works only in the branches of the main repository, so for PRs - else case runs

  if [ ! "$CACHIX_SIGNING_KEY" = "" ]

    then

      # Build of the inside repo branch - enable push Cachix cache
      BUILD_PROJECT | cachix push "$cachixAccount"

    else

      # Build of the side repo/PR - can not push Cachix cache
      BUILD_PROJECT

  fi

}

# Run the entry function of the script
MAIN
