#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

echo "========================================"
echo "  HNix Parser Comparison Test Suite"
echo "========================================"
echo ""

TOTAL_PASS=0
TOTAL_FAIL=0

run_phase() {
  local script="$1"
  local name="$2"

  echo "----------------------------------------"
  echo "Running: $name"
  echo "----------------------------------------"

  if bash "$script"; then
    echo ""
    echo "* $name: PASSED"
    ((TOTAL_PASS++)) || true
  else
    echo ""
    echo "X $name: FAILED"
    ((TOTAL_FAIL++)) || true
  fi
  echo ""
}

run_phase "01-string-escapes.sh" "Phase 1: String Escapes"
run_phase "02-nix-test-suite.sh" "Phase 2: Nix Test Suite"
run_phase "03-edge-cases.sh" "Phase 3: Edge Cases"

echo "========================================"
echo "  FINAL RESULTS"
echo "========================================"
echo "Phases Passed: $TOTAL_PASS"
echo "Phases Failed: $TOTAL_FAIL"
echo ""

if [[ $TOTAL_FAIL -eq 0 ]]; then
  echo "* ALL TESTS PASSED"
  exit 0
else
  echo "X SOME TESTS FAILED"
  exit 1
fi
