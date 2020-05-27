#!/usr/bin/env bash

# NOTE: Script for the CI builds. CI comes here from `.travis.yml`

set -xe
set -euo pipefail
IFS=$'\n\t'

if [ "$GHCVERSION" = "ghcjs" ]; then
    nix-build --substituters 'https://nixcache.reflex-frp.org?trusted=1' ghcjs
else
    nix-build                                   \
        --argstr compiler $GHCVERSION           \
        --arg doTracing $TRACING                \
        --arg doStrict $STRICT                  \
        $@
fi
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

