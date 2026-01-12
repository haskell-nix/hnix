#!/usr/bin/env bash

# HNix version of nixpkgs/lib/tests/modules.sh
# Tests the module system using HNix instead of nix-instantiate

set -o errexit -o noclobber -o nounset -o pipefail
shopt -s failglob inherit_errexit

NIXPKGS_DIR="${NIXPKGS_DIR:-/home/connorbaker/nixpkgs}"
HNIX_BIN="${HNIX_BIN:-/home/connorbaker/hnix/dist-newstyle/build/x86_64-linux/ghc-9.10.3/hnix-0.17.0/x/hnix/build/hnix/hnix}"

cd "$NIXPKGS_DIR/lib/tests/modules"

pass=0
fail=0

hnix-eval() {
    "$HNIX_BIN" --eval --strict "$@" 2>&1
}

logFailure() {
    printf '\033[1;31mFAIL\033[0m: %s\n' "$*"
}

logSuccess() {
    printf '\033[1;32mPASS\033[0m: %s\n' "$*"
}

evalConfig() {
    local attr=$1
    shift
    local script="(import ./default.nix { modules = [ $* ];}).$attr"
    hnix-eval --expr "$script" 2>&1 | grep -v "^evaluation warning:"
}

checkConfigOutput() {
    local outputContains=$1
    shift
    local output
    if output=$(evalConfig "$@" 2>&1) && echo "$output" | grep -E --silent "$outputContains" ; then
        ((++pass))
        logSuccess "$2"
    else
        ((++fail))
        logFailure "$2"
        echo "  Expected: $outputContains"
        echo "  Got: $(echo "$output" | head -1 | cut -c1-80)"
    fi
}

checkConfigError() {
    local errorContains=$1
    shift
    local err=""
    if err="$(evalConfig "$@" 2>&1)"; then
        if echo "$err" | grep -zP --silent "$errorContains" ; then
            ((++pass))
            logSuccess "$2 (error check)"
        else
            ((++fail))
            logFailure "$2 (error check)"
        fi
    else
        if echo "$err" | grep -zP --silent "$errorContains" ; then
            ((++pass))
            logSuccess "$2 (error check)"
        else
            ((++fail))
            logFailure "$2 (error check)"
        fi
    fi
}

echo "Running module system tests with HNix..."
echo

# Basic module tests
checkConfigOutput '^"one two"$' config.result ./shorthand-meta.nix
checkConfigOutput '^true$' config.result ./test-mergeAttrDefinitionsWithPrio.nix
checkConfigOutput '^true$' config.result ./module-argument-default.nix
checkConfigOutput '"ok"' config.result ./specialArgs-lib.nix

# types.attrTag
checkConfigOutput '^true$' config.okChecks ./types-attrTag.nix

# types
checkConfigOutput '"ok"' config.assertions ./types.nix

# Check boolean option
checkConfigOutput '^false$' config.enable ./declare-enable.nix
checkConfigError 'The option .* does not exist' config.enable ./define-enable.nix

# Check boolByOr type
checkConfigOutput '^false$' config.value.falseFalse ./boolByOr.nix
checkConfigOutput '^true$' config.value.trueFalse ./boolByOr.nix
checkConfigOutput '^true$' config.value.falseTrue ./boolByOr.nix
checkConfigOutput '^true$' config.value.trueTrue ./boolByOr.nix

# bare-submodule tests
checkConfigOutput '^1$' config.bare-submodule.nested ./declare-bare-submodule.nix ./declare-bare-submodule-nested-option.nix
checkConfigOutput '^2$' config.bare-submodule.deep ./declare-bare-submodule.nix ./declare-bare-submodule-deep-option.nix

# Check integer types
checkConfigOutput '^42$' config.value ./declare-int-unsigned-value.nix ./define-value-int-positive.nix
checkConfigError 'not of type.*unsigned integer' config.value ./declare-int-unsigned-value.nix ./define-value-int-negative.nix

# Check either types
checkConfigOutput '^42$' config.value ./declare-either.nix ./define-value-int-positive.nix
checkConfigOutput '^"24"$' config.value ./declare-either.nix ./define-value-string.nix

# Check mkForce
checkConfigOutput '^true$' config.enable ./declare-enable.nix ./define-enable.nix
checkConfigOutput '^false$' config.enable ./declare-enable.nix ./define-enable.nix ./define-force-enable.nix

# Check oneOf
checkConfigOutput '^42$' config.value ./declare-oneOf.nix ./define-value-int-positive.nix
checkConfigOutput '^\[\]$' config.value ./declare-oneOf.nix ./define-value-list.nix
checkConfigOutput '^"24"$' config.value ./declare-oneOf.nix ./define-value-string.nix

# Print summary
echo
echo "========================================"
echo "SUMMARY: $pass passed, $fail failed"
echo "========================================"

if [[ $fail -gt 0 ]]; then
    exit 1
else
    echo "All tests passed!"
fi
