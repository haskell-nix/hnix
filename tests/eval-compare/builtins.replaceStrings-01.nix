with builtins;

let a1 = toFile "foo" "foo contents"; # /nix/store/pqwdc5m06lxl8gmzcd26ifwsdhq9fj7k-foo
    a2 = toFile "bar" "bar contents"; # /nix/store/4q6kxj1ym13yfp1bcdrzrwa1la6dqgp5-bar
    b = dirOf a1;
    c = substring 3 1 b;
    d = replaceStrings ["b"] [c] "abc";
    e = replaceStrings ["k"] [c] "abc";
    f = replaceStrings ["y"] [c] (dirOf a2);
    g = replaceStrings ["s"] [c] (dirOf a2);
    h = replaceStrings ["y"] ["z"] "abc";
in [ b c d e f g h     # TODO Add a1 here when we have correct store hashing working
     (hasContext d)
     (hasContext e)
     (hasContext f)
     (hasContext g)
     (hasContext h)
   ]