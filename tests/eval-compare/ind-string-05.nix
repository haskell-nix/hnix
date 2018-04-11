let

  s5 = ''
      The \ is not special here.
    ' can be followed by any character except another ', e.g. 'x'.
    Likewise for $, e.g. $$ or $varName.
    But ' followed by ' is special, as is $ followed by {.
    If you want them, use anti-quotations: ${"''"}, ${"\${"}.
  '';

in s5
