let f = { a = 1; b = import ./hello.nix; }; in f.a
