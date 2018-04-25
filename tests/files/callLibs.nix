let callLibs = file: import file { lib = self; };
    trivial = callLibs ./trivial.nix;
in trivial
