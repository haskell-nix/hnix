let
  drv = derivation {
    name = "fail";
    builder = "/bin/false";
    system = "x86_64-linux";
    outputs = [ "out" "foo" ];
  };

  path = "${./builtins.appendContext.nix}";

  desired-context = {
    "${builtins.unsafeDiscardStringContext path}" = {
      path = true;
    };
    "${builtins.unsafeDiscardStringContext drv.drvPath}" = {
      outputs = [ "foo" "out" ];
      allOutputs = true;
    };
  };

  # TODO: Remove builtins.attrValues here once store hash is correct.
  legit-context = builtins.attrValues (builtins.getContext "${path}${drv.outPath}${drv.foo.outPath}${drv.drvPath}");

  constructed-context = builtins.attrValues (builtins.getContext (builtins.appendContext "" desired-context));
in [ (builtins.appendContext "foo" {})
     (legit-context == constructed-context)
     constructed-context
   ]
