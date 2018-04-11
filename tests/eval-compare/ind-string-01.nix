let

  s1 = ''
    This is an indented multi-line string
    literal.  An amount of whitespace at
    the start of each line matching the minimum
    indentation of all lines in the string
    literal together will be removed.  Thus,
    in this case four spaces will be
    stripped from each line, even though
      THIS LINE is indented six spaces.

    Also, empty lines don't count in the
    determination of the indentation level (the
    previous empty line has indentation 0, but
    it doesn't matter).
  '';

in s1
