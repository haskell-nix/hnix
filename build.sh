#!/bin/bash -xe

set -euo pipefail
IFS=$'\n\t'

GHCVERSION=${GHCVERSION:-ghc822}
STRICT=${STRICT:-false}
TRACING=${TRACING:-false}

if [ "$GHCVERSION" = "ghcjs" ]; then
    nix-build --substituters 'https://nixcache.reflex-frp.org?trusted=1' ghcjs
else
    nix-build                                   \
        --argstr compiler $GHCVERSION           \
        --arg doTracing $TRACING                \
        --arg doStrict $STRICT
fi
