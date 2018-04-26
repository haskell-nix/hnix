let
  fix = f: let x = f x; in x;
  extends = f: rattrs: self: let super = rattrs self; in super // f self super;

  allPackages = newArgs:
    let allPackages = pkgs: _: {
          pkgs = pkgs;
          darwin = pkgs.darwin; # THUNK FORCE LOOPS: self-reference
        };
    in fix (builtins.foldl' (x: y: extends y x) (_: {}) [
         (_: _: { inherit (newArgs) stdenv; })
         allPackages
         (_: super: super.stdenv.overrides super)
       ]);

  stage0 = _: { stdenv = { overrides = x: x; }; };
  stage1 = p: with p; pkgs.darwin; # THUNK FORCE LOOP: CREATED

  xs = [ stage1 stage0 ];
  go = n:
    if n == builtins.length xs
    then {}
    else allPackages (builtins.elemAt xs n (go (n + 1)));

in go 0
