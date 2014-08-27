{
  a = 3;
  b = 4;
  c = {
    inherit ({ a = 3; }) a;
    e = 4;
    f = 5;
  };
  d = if true
    then { a = 1; b = 2; c = 3; }
    else if false
      then null
      else {
        inherit ({
          a = 3;
          b = 4;
          cdefgads = 5;
        }) cdefgads;
      };
  f = x: x;
  list = [
    1
    2
    (f 3)
    ((x: x) 3)
    [ 4 5 ]
  ];
}
