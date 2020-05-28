#!/usr/bin/env bash

# NOTE: Script for the CI builds. CI comes here from `.travis.yml`

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


if [ "$generateOptparseApplicativeCompletion" = 'true' ]
  then
    # NOTE: Enable shell complition generation
    generateOptparseApplicativeCompletion="--arg generateOptparseApplicativeCompletion $name $pkgName"
  else
    # NOTE: Skip the shell complition generation
    generateOptparseApplicativeCompletion=''
fi


if [ "$GHCVERSION" = "ghcjs" ]
  then

    # NOTE: GHC JS build
    # Bu itselt GHC JS build every tine creates >65000 line logs that are >4MB in size, so Travis terminates due to log size quota.
    # nixbuild --quiet (x5) does not work on GHC JS build
    # So there was a need to make it build.
    # Solution - is to silence the stdout
    # But Travis then terminates on 10 min no stdout timeout
    # so HACK: SILENT wrapper allows to surpress the huge log, while still preserves the Cachix caching ability in any case of the build
    # On build failure outputs the last 10000 lines of log (that should be more then enough), and terminates
    SILENT nix-build                                         \
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
      ghcjs \
      "$@"

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
