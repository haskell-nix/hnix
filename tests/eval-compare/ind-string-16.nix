let

  # Regression test: accept $'.
  s16 = ''
    cut -d $'\t' -f 1
  '';

in s16
