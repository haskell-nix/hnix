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
GHCVERSION=${GHCVERSION:-ghc865}
fi
