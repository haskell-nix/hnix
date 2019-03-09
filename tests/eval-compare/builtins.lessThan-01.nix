with builtins;

let a = 4.000000000000000000001;
    b = 4;
    c = "abcd";
    d = "abce";
    e = -4.1;
    f = -4;
    g = 0;
    h = -0;
    i = 999999999999999999;     # note: this is max bound
    j = 999999999999999998;
    k = -999999999999999999;    # note: this is min bound
    l = -999999999999999998;
in [(lessThan a b)
    (lessThan b a)
    (lessThan c d)
    (lessThan d c)
    (lessThan e f)
    (lessThan f e)
    (lessThan g h)
    (lessThan h g)
    (lessThan i j)
    (lessThan j i)
    (lessThan k l)
    (lessThan l k)
  ]
