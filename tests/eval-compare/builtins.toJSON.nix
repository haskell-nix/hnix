with builtins;

let f = toFile "foo" "foo contents"; # /nix/store/pqwdc5m06lxl8gmzcd26ifwsdhq9fj7k-foo
    objA = { a = 15; b = substring 1 3 (dirOf f); };
    objB = { a = 42; b = "hello"; };
in [ (hasContext (toJSON objA))
     (hasContext (toJSON objB))
   ]