#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
RESULTS_DIR="$SCRIPT_DIR/results"
NIX_TESTS="$HOME/nix/tests/functional/lang"
mkdir -p "$RESULTS_DIR"

echo "=== Phase 2: Nix Test Suite Comparison ==="

# String-related test files
TEST_FILES=(
  "eval-okay-string.nix"
  "eval-okay-ind-string.nix"
  "eval-okay-backslash-newline-1.nix"
  "eval-okay-backslash-newline-2.nix"
)

cd /home/connorbaker/hnix

# Arrays to store valid test names and paths
declare -a VALID_TESTS=()
declare -a VALID_PATHS=()
declare -a SKIPPED=()

# Phase 1: Filter valid tests and run Nix on them
echo "Running Nix evaluations..."
for test_file in "${TEST_FILES[@]}"; do
  test_path="$NIX_TESTS/$test_file"

  if [[ ! -f "$test_path" ]]; then
    SKIPPED+=("$test_file (not found)")
    continue
  fi

  # Run Nix
  nix_out="$RESULTS_DIR/nix-${test_file%.nix}.out"
  if ! nix-instantiate --eval --strict "$test_path" > "$nix_out" 2>&1; then
    SKIPPED+=("$test_file (Nix eval failed)")
    continue
  fi

  VALID_TESTS+=("$test_file")
  VALID_PATHS+=("$test_path")
done

echo "  Found ${#VALID_TESTS[@]} valid tests, ${#SKIPPED[@]} skipped"

# Phase 2: Run HNix on all valid tests in a single nix develop invocation
if [[ ${#VALID_TESTS[@]} -gt 0 ]]; then
  echo "Running HNix evaluations (single nix develop session)..."

  # Build first to avoid build output in test results
  nix develop ".?submodules=1#" --command cabal build exe:hnix >/dev/null 2>&1

  # Run all HNix evaluations in one nix develop session using cabal exec
  nix develop ".?submodules=1#" --command bash -c '
    results_dir="$1"
    shift
    for test_path in "$@"; do
      test_file=$(basename "$test_path")
      out_file="$results_dir/hnix-${test_file%.nix}.out"
      if cabal exec hnix -- --eval --strict "$test_path" > "$out_file" 2>&1; then
        echo "OK: $test_file"
      else
        echo "FAIL: $test_file"
        echo "" > "$out_file"
      fi
    done
  ' _ "$RESULTS_DIR" "${VALID_PATHS[@]}"
fi

# Phase 3: Compare results
echo ""
echo "=== Results ==="

PASS=0
FAIL=0

for test_file in "${VALID_TESTS[@]}"; do
  nix_out="$RESULTS_DIR/nix-${test_file%.nix}.out"
  hnix_out="$RESULTS_DIR/hnix-${test_file%.nix}.out"

  if [[ ! -s "$hnix_out" ]]; then
    echo "FAIL: $test_file (HNix eval failed)"
    ((FAIL++)) || true
  elif diff -q "$nix_out" "$hnix_out" >/dev/null 2>&1; then
    echo "PASS: $test_file"
    ((PASS++)) || true
  else
    echo "FAIL: $test_file"
    nix_norm=$(tr -d '\n' < "$nix_out" | tr -s ' ')
    hnix_norm=$(tr -d '\n' < "$hnix_out" | tr -s ' ')
    echo "  Nix:  ${nix_norm:0:100}..."
    echo "  HNix: ${hnix_norm:0:100}..."
    ((FAIL++)) || true
  fi
done

# Report skipped
for skip in "${SKIPPED[@]}"; do
  echo "SKIP: $skip"
done

echo ""
echo "=== Summary ==="
echo "PASS: $PASS"
echo "FAIL: $FAIL"
echo "SKIP: ${#SKIPPED[@]}"

[[ $FAIL -eq 0 ]] || exit 1
