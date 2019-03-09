with builtins;

let a = fetchurl "https://haskell.org";

in [ a (hasContext a) ]
