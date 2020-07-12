#!/usr/bin/env bash

# NOTE: Script for the CI builds, relies on `default.nix` interface, which exposes Nixpkgs Haskell Lib interface

# The most strict error checking requirements
set -Eexuo pipefail

### NOTE: Section handles imports from env, these are settings for Nixpkgs.
### They use the `default.nix` interface, which exposes expose most of the Nixpkgs Haskell.lib API: https://github.com/NixOS/nixpkgs/blob/master/pkgs/development/haskell-modules/lib.nix
### Some of these options implicitly switch the dependent options.
### Documentation of this settings is mosly in `default.nix`, since most settings it Nixpkgs related
### Additional documentation is in Nixpkgs Haskell.lib: https://github.com/NixOS/nixpkgs/blob/master/pkgs/development/haskell-modules/lib.nix


# NOTE: If vars not imported - init the vars with default values
compiler=${compiler:-'ghc883'}

packageName=${packageName:-'defaultPackageName'}
packageRoot=${packageRoot:-'pkgs.nix-gitignore.gitignoreSource [ ] ./.'}
useRev=${useRev:-'false'}
rev=${rev:-'nixpkgs-unstable'}
# Account in Cachix to use
cachixAccount=${cachixAccount:-'replaceWithProjectNameInCachix'}


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
executableNamesToShellComplete=${executableNamesToShellComplete:-'[ "replaceWithExecutableName" ]'}


withHoogle=${withHoogle:-'false'}

# Log file to dump GHCJS build into
ghcjsTmpLogFile=${ghcjsTmpLogFile:-'/tmp/ghcjsTmpLogFile.log'}
# Length of the GHCJS log tail (<40000)
ghcjsLogTailLength=${ghcjsLogTailLength:-'10000'}

# If key not provided (branch is not inside the central repo) - init CACHIX_SIGNING_KEY as empty
CACHIX_SIGNING_KEY=${CACHIX_SIGNING_KEY:-""}


GHCJS_BUILD(){
# NOTE: Function for GHCJS build that outputs its huge log into a file

  # Run the build into Log (log is too long for Travis)
  "$@" &> "$ghcjsTmpLogFile"

}

SILENT(){
# NOTE: Function that silences the build process
# In normal mode outputs only the /nix/store paths

  echo "Log: $ghcjsTmpLogFile"
  # Pass into the ghcjsbuild function the build command
  if GHCJS_BUILD "$@"
  then

    # Output log lines for stdout -> cachix caching
    grep '^/nix/store/' "$ghcjsTmpLogFile"

  else

    # Output log lines for stdout -> cachix caching
    grep '^/nix/store/' "$ghcjsTmpLogFile"

    # Propagate the error state, fail the CI build
    exit 1

  fi

}

BUILD_PROJECT(){


IFS=$'\n\t'

if [ "$compiler" = "ghcjs" ]
  then

    # GHCJS build
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
      "$compiler"

  else

    # Normal GHC build
    # GHC sometimes produces logs so big - that Travis terminates builds, so multiple --quiet
    nix-build \
      --quiet --quiet \
      --argstr compiler "$compiler" \
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
