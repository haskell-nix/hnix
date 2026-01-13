#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
TEST_DIR="$SCRIPT_DIR/fixtures"
RESULTS_DIR="$SCRIPT_DIR/results"
mkdir -p "$TEST_DIR" "$RESULTS_DIR"

# Create test fixture - output as JSON for consistent comparison
cat > "$TEST_DIR/string-escapes.nix" << 'NIXEOF'
builtins.toJSON {
  # === Double-quoted string escapes ===
  dq_newline = "\n";
  dq_carriage = "\r";
  dq_tab = "\t";
  dq_quote = "\"";
  dq_dollar = "\$";
  dq_backslash = "\\";
  dq_unknown_escape = "\x";  # Should produce "x"
  dq_dollar_brace = "\${";   # Should produce "${"

  # === Indented string escapes ===
  ind_double_quote = '''';                # Should produce "''"
  ind_dollar = ''$'';                     # Should produce "$"
  ind_escaped_newline = ''\n'';           # Escaped newline -> "\n"
  ind_escaped_tab = ''\t'';               # Escaped tab -> "\t"
  ind_escaped_backslash = ''\'';          # Escaped backslash -> "\"
  ind_unknown_escape = ''\x'';            # Should produce "x"

  # === Dollar sign patterns ===
  dollar_alone = "$";
  dollar_double = "$$";
  dollar_triple = "$$$";
  dollar_quad = "$$$$";
  dollar_before_text = "$foo";
  dollar_before_brace = "$${foo}";        # Should be literal "$${foo}"
  dollar_triple_interp = let x = 1; in "$$${ toString x }";  # $$ then interpolation -> "$$1"

  # === Interpolation ===
  interp_simple = "${toString 42}";
  interp_in_indented = ''${toString 42}'';

  # === String lengths for verification ===
  len_dollar_alone = builtins.stringLength "$";           # 1
  len_dollar_double = builtins.stringLength "$$";         # 2
  len_dollar_triple = builtins.stringLength "$$$";        # 3
  len_dollar_brace = builtins.stringLength "$${foo}";     # 7: $ $ { f o o }
}
NIXEOF

echo "=== Phase 1: String Escape Comparison ==="

# Run Nix
echo "Running nix-instantiate..."
if nix-instantiate --eval --strict "$TEST_DIR/string-escapes.nix" > "$RESULTS_DIR/nix-escapes.out" 2>&1; then
  echo "  Nix: OK"
else
  echo "  Nix: FAILED"
  cat "$RESULTS_DIR/nix-escapes.out"
  exit 1
fi

# Run HNix (suppress build output, only capture eval result)
echo "Running hnix..."
cd /home/connorbaker/hnix
if nix develop ".?submodules=1#" --command \
   cabal run hnix -- --eval --strict "$TEST_DIR/string-escapes.nix" 2>/dev/null > "$RESULTS_DIR/hnix-escapes.out"; then
  echo "  HNix: OK"
else
  echo "  HNix: FAILED"
  cat "$RESULTS_DIR/hnix-escapes.out"
  exit 1
fi

# Compare (normalize JSON output by parsing and re-serializing)
echo "Comparing outputs..."

# Extract just the JSON string (remove outer quotes from nix eval output)
nix_json=$(cat "$RESULTS_DIR/nix-escapes.out" | sed 's/^"//;s/"$//' | sed 's/\\"/"/g')
hnix_json=$(cat "$RESULTS_DIR/hnix-escapes.out" | sed 's/^"//;s/"$//' | sed 's/\\"/"/g')

# Compare the JSON content
if [[ "$nix_json" == "$hnix_json" ]]; then
  echo "  PASS: Outputs match"
else
  echo "  FAIL: Outputs differ"
  echo "--- Nix output:"
  echo "$nix_json" | head -5
  echo "..."
  echo "--- HNix output:"
  echo "$hnix_json" | head -5
  echo "..."
  echo ""
  echo "Full diff saved to: $RESULTS_DIR/escapes-diff.out"
  diff <(echo "$nix_json" | jq -S . 2>/dev/null || echo "$nix_json") \
       <(echo "$hnix_json" | jq -S . 2>/dev/null || echo "$hnix_json") \
       > "$RESULTS_DIR/escapes-diff.out" 2>&1 || true
  cat "$RESULTS_DIR/escapes-diff.out"
  exit 1
fi
