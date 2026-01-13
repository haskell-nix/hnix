#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
TEST_DIR="$SCRIPT_DIR/fixtures"
RESULTS_DIR="$SCRIPT_DIR/results"
mkdir -p "$TEST_DIR" "$RESULTS_DIR"

echo "=== Phase 3: Edge Case Tests ==="

# Test 1: Unicode handling
cat > "$TEST_DIR/unicode.nix" << 'NIXEOF'
{
  ascii = "hello";
  unicode_simple = "hello";
  unicode_emoji = "hello";
  unicode_cjk = "hello";
  unicode_in_interp = "hello ${"world"}";
  unicode_in_indented = '' hello world '';
}
NIXEOF

# Test 2: Deeply nested interpolation
cat > "$TEST_DIR/nested-interp.nix" << 'NIXEOF'
let
  a = "a";
  b = "b";
  c = "c";
in {
  depth1 = "${a}";
  depth2 = "${a}${b}";
  depth3 = "${a}${b}${c}";
  nested_concat = "${a + b + c}";
  nested_if = "${if true then a else b}";
}
NIXEOF

# Test 3: Indentation edge cases
cat > "$TEST_DIR/indentation.nix" << 'NIXEOF'
{
  # Empty indented string
  empty = '''';

  # Single line
  single = ''hello'';

  # Trailing newline
  trailing = ''
    hello
  '';

  # No trailing newline
  no_trailing = ''  hello'';

  # Interpolation affects indentation
  interp_indent = ''
    before
    ${toString 42}
    after
  '';

  # Escaped newline in indentation context
  escaped_nl = ''
    line1''\nline2
  '';
}
NIXEOF

# Test 4: Path edge cases
cat > "$TEST_DIR/paths.nix" << 'NIXEOF'
{
  simple_path = ./foo;
  absolute_path = /tmp/test;
  home_path = ~/test;
  path_with_dots = ./foo/../bar;
}
NIXEOF

# Test 5: CR/LF handling
cat > "$TEST_DIR/crlf.nix" << 'NIXEOF'
{
  normal_newline = "line1\nline2";
}
NIXEOF

cd /home/connorbaker/hnix

TEST_NAMES=(unicode nested-interp indentation paths crlf)

# Phase 1: Run Nix evaluations
echo "Running Nix evaluations..."
for name in "${TEST_NAMES[@]}"; do
  nix_out="$RESULTS_DIR/nix-$name.out"
  if nix-instantiate --eval --strict "$TEST_DIR/$name.nix" > "$nix_out" 2>&1; then
    echo "  OK: $name"
  else
    echo "  FAIL: $name"
  fi
done

# Phase 2: Run HNix evaluations in single nix develop session
echo "Running HNix evaluations (single nix develop session)..."
nix develop ".?submodules=1#" --command bash -c '
  test_dir="$1"
  results_dir="$2"
  shift 2
  for name in "$@"; do
    hnix_out="$results_dir/hnix-$name.out"
    if cabal run hnix -- --eval --strict "$test_dir/$name.nix" > "$hnix_out" 2>/dev/null; then
      echo "  OK: $name"
    else
      echo "  FAIL: $name"
      echo "" > "$hnix_out"
    fi
  done
' _ "$TEST_DIR" "$RESULTS_DIR" "${TEST_NAMES[@]}"

# Phase 3: Compare results
echo ""
echo "=== Results ==="

normalize_output() {
  # Remove all whitespace, braces, then split on semicolons, sort, rejoin
  tr -d '\n' < "$1" | \
    sed 's/^[{ ]*//; s/[} ]*$//; s/;[[:space:]]*/\n/g' | \
    grep -v '^$' | \
    sort
}

PASS=0
FAIL=0

for name in "${TEST_NAMES[@]}"; do
  nix_out="$RESULTS_DIR/nix-$name.out"
  hnix_out="$RESULTS_DIR/hnix-$name.out"

  if [[ ! -s "$hnix_out" ]]; then
    echo "FAIL: $name (HNix eval failed)"
    ((FAIL++)) || true
    continue
  fi

  nix_sorted=$(normalize_output "$nix_out")
  hnix_sorted=$(normalize_output "$hnix_out")

  if [[ "$nix_sorted" == "$hnix_sorted" ]]; then
    echo "PASS: $name"
    ((PASS++)) || true
  else
    echo "FAIL: $name"
    diff <(echo "$nix_sorted") <(echo "$hnix_sorted") | head -10
    ((FAIL++)) || true
  fi
done

echo ""
echo "=== Summary ==="
echo "PASS: $PASS"
echo "FAIL: $FAIL"

[[ $FAIL -eq 0 ]] || exit 1
