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
