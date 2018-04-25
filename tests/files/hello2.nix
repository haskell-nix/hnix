let x = { z = x: import ./eighty.nix + 20 + x; w = 123; };
            allPackages = self:
              super:
                let
                  res = import ./eighty.nix {
                    inherit lib nixpkgsFun noSysDirs
                            config;
                  } null self;
                in res;
    y = "Hello";
    z = "Goodbye";
    f = x: if x == 0 then x * 2 else x + 2;
    w = x.z 5 + f 3 - 15;
in assert w == 1;
   if x.z 2 == 100 then y else 3
