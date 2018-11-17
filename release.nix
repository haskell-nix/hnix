{}:
let matrix = [
      { compiler = "ghc802"; doStrict = false; doTracing = false; }
      { compiler = "ghc802"; doStrict = false; doTracing = true;  }
      { compiler = "ghc822"; doStrict = true;  doTracing = false; }
      { compiler = "ghc822"; doStrict = true;  doTracing = true;  }
      { compiler = "ghc843"; doStrict = false; doTracing = false; }
      { compiler = "ghc843"; doStrict = false; doTracing = true;  }

      # Broken
      # { compiler = "ghcjs";  doStrict = false; doTracing = false; }
    ];
    boolToString = x: if x then "true" else "false";
    nameForConfig = {compiler, doStrict, doTracing}: builtins.concatStringsSep "-"
      [ compiler (boolToString doStrict) (boolToString doTracing) ];
in builtins.listToAttrs (map (args: { name = nameForConfig args; value = import ./. args; }) matrix)
