#!/bin/bash -xe

set -euo pipefail
IFS=$'\n\t'

GHCVERSION=${GHCVERSION:-ghc822}
STRICT=${STRICT:-false}
TRACING=${TRACING:-false}
PROFILING=${PROFILING:-false}

if [ "$GHCVERSION" = "ghcjs" ]; then
    exec nix-build ghcjs
else
    exec nix-build                              \
        --argstr compiler $GHCVERSION           \
        --arg doProfiling $PROFILING            \
        --arg doTracing $TRACING                \
        --arg doStrict $STRICT
fi
