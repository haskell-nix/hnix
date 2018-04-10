let

  # Regression test: antiquotation in '${x}' should work, but didn't.
  s15 = let x = "bla"; in ''
    foo
    '${x}'
    bar
  '';

in s15
