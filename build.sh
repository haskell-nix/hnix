#!/usr/bin/env bash

# NOTE: Script for the CI builds. CI comes here from `.travis.yml`

GHCJS_BUILD(){
# NOTE: Function for GHCJS build that outputs its huge log into a file

  # NOTE: Run the build into Log (log is too long for Travis)
  "$@" &> "$ghcjsTmpLogFile"

}

BUILD_PROJECT(){
set -xe
set -euo pipefail
IFS=$'\n\t'

# NOTE: If var not imported - set to the default value
GHCVERSION=${GHCVERSION:-ghc865}
rev=${rev:-nixpkgs-unstable}
NIX_PATH=${NIX_PATH:-"nixpkgs=https://github.com/nixos/nixpkgs/archive/$rev.tar.gz"}
export NIX_PATH
name=${name:-defaultBinaryName}
pkgName=${pkgName:-defaultPkgName}
failOnAllWarnings=${failOnAllWarnings:-'false'}
checkUnusedPackages=${checkUnusedPackages:-'false'}
doCoverage=${doCoverage:-'false'}
doHaddock=${doHaddock:-'false'}
doJailbreak=${doJailbreak:-'false'}
doCheck=${doCheck:-'true'}
doBenchmark=${doBenchmark:-'false'}
enableExecutableProfiling=${enableExecutableProfiling:-'false'}
enableLibraryProfiling=${enableLibraryProfiling:-'false'}
buildFromSdist=${buildFromSdist:-'false'}
buildStrictly=${buildStrictly:-'false'}
disableOptimization=${disableOptimization:-'true'}
buildStackProject=${buildStackProject:-'false'}
# NOTE: *Oprparse* key is redifined in the code further
generateOptparseApplicativeCompletion=${generateOptparseApplicativeCompletion:-'false'}
allowInconsistentDependencies=${allowInconsistentDependencies:-'false'}
ghcjsTmpLogFile=${ghcjsTmpLogFile:-'/tmp/ghcjsTmpLogFile.jog'}
ghcjsLogTailLength=${ghcjsLogTailLength:-'10000'}

if [ "$GHCVERSION" = "ghcjs" ]; then
    nix-build --substituters 'https://nixcache.reflex-frp.org?trusted=1' ghcjs
  else

    # NOTE: Normal GHC build
    # NOTE: GHC sometimes produces logs so big - that Travis terminates builds, so multiple --quiet
    nix-build                                                \
      --quiet --quiet                                        \
      --argstr compiler "$GHCVERSION"                        \
      --arg failOnAllWarnings "$failOnAllWarnings"           \
      --arg buildStrictly "$buildStrictly"                   \
      --arg checkUnusedPackages "$checkUnusedPackages"       \
      --arg doCoverage "$doCoverage" \
      --arg doHaddock "$doHaddock" \
      --arg doJailbreak "$doJailbreak" \
      --arg doCheck "$doCheck" \
      --arg doBenchmark "$doBenchmark" \
      --arg enableExecutableProfiling "$enableExecutableProfiling" \
      --arg enableLibraryProfiling "$enableLibraryProfiling" \
      --arg buildFromSdist "$buildFromSdist" \
      --arg buildStrictly "$buildStrictly" \
      --arg disableOptimization "$disableOptimization" \
      --arg buildStackProject "$buildStackProject" \
      "$generateOptparseApplicativeCompletion" \
      --arg allowInconsistentDependencies "$allowInconsistentDependencies" \
      "$@"

fi
}

MAIN() {

# NOTE: Secrets are not shared to PRs from forks
# NOTE: nix-build | cachix push <name> - uploads binaries, runs&works only in the branches of the main repository, so for PRs - else case runs

  if [ ! "$CACHIX_SIGNING_KEY" = "" ]

    then

      # NOTE: Build of the inside repo branch - enable push Cachix cache
      BUILD_PROJECT | cachix push "$name"

    else

      # NOTE: Build of the side repo/PR - can not push Cachix cache
      BUILD_PROJECT

  fi

}

# NOTE: Run the entry function of the script
MAIN

