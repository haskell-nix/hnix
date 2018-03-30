let x = rec {

  y = 2;
  z = { w = 4; };
  v = rec {
    u = 6;
    t = [ u z.w s.q ];
  };
  s = { r = import ./goodbye.nix; q = 10; };
  p = import ./hello.nix;

}; o = 100; in [ x.v.t x.z.w x.p x.p ]
