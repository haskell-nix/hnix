#!/usr/bin/env bash

set -xe
set -euo pipefail
IFS=$'\n\t'

GHCVERSION=${GHCVERSION:-ghc822}
STRICT=${STRICT:-false}
TRACING=${TRACING:-false}
NIX_PATH=nixpkgs=https://github.com/NixOS/nixpkgs/archive/6820e2f0dd16104961d6fc7e8e38846807159c4e.tar.gz

if [ "$GHCVERSION" = "ghcjs" ]; then
    nix-build --substituters 'https://nixcache.reflex-frp.org?trusted=1' ghcjs
else
    nix-build                                   \
        --argstr compiler $GHCVERSION           \
        --arg doTracing $TRACING                \
        --arg doStrict $STRICT                  \
        $@
fi
