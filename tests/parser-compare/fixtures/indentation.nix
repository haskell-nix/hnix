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
