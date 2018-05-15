#!/bin/bash -xe

set -euo pipefail
IFS=$'\n\t'

GHCVERSION=${GHCVERSION:-ghc822}
STRICT=${STRICT:-false}
TRACING=${TRACING:-false}
PROFILING=${PROFILING:-false}

if [ "$GHCVERSION" = "ghcjs" ]; then
    NIX_CONF_DIR=$PWD/ghcjs nix-build ghcjs
else
    nix-build                                   \
        --argstr compiler $GHCVERSION           \
        --arg doProfiling $PROFILING            \
        --arg doTracing $TRACING                \
        --arg doStrict $STRICT
fi
