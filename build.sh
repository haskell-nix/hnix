#!/usr/bin/env bash

set -xe
set -euo pipefail
IFS=$'\n\t'

GHCVERSION=${GHCVERSION:-ghc863}
STRICT=${STRICT:-false}
TRACING=${TRACING:-false}
NIX_PATH=nixpkgs=https://github.com/NixOS/nixpkgs/archive/120eab94e0981758a1c928ff81229cd802053158.tar.gz

if [ "$GHCVERSION" = "ghcjs" ]; then
    nix-build --substituters 'https://nixcache.reflex-frp.org?trusted=1' ghcjs
else
    nix-build                                   \
        --argstr compiler $GHCVERSION           \
        --arg doTracing $TRACING                \
        --arg doStrict $STRICT                  \
        $@
fi
