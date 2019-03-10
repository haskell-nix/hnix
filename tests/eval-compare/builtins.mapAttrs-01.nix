with builtins;

let fooset = { foo = 123; bar = 456; };
    lolset = { "foo/bar" = "lol"; "bar/baz" = "wat";};
    emptyset = {};
in [ (all (x: hasContext(x)) (attrValues (mapAttrs (key: value: key) fooset)))
     (all (x: hasContext(x)) (attrValues (mapAttrs (key: value: key) lolset)))
     (all (x: hasContext(x)) (attrValues (mapAttrs (key: value: key) emptyset)))
     (all (x: hasContext(x)) (attrValues (mapAttrs (key: value: key + toString value) fooset)))
     (all (x: hasContext(x)) (attrValues (mapAttrs (key: value: key + toString value) lolset)))
     (all (x: hasContext(x)) (attrValues (mapAttrs (key: value: key + toString value) emptyset)))
     (all (x: hasContext(x)) (attrValues (mapAttrs (key: value: key + toJSON value) fooset)))
     (all (x: hasContext(x)) (attrValues (mapAttrs (key: value: key + toJSON value) lolset)))
     (all (x: hasContext(x)) (attrValues (mapAttrs (key: value: key + toJSON value) emptyset)))
     (all (x: hasContext(x)) (attrValues (mapAttrs (key: value: key + toJSON (toString value)) fooset)))
     (all (x: hasContext(x)) (attrValues (mapAttrs (key: value: key + toJSON (toString value)) lolset)))
     (all (x: hasContext(x)) (attrValues (mapAttrs (key: value: key + toJSON (toString value)) emptyset)))
     (all (x: hasContext(x)) (attrValues (mapAttrs (key: value: key + value) lolset)))
     (all (x: hasContext(x)) (attrValues (mapAttrs (key: value: key + value) emptyset)))
  ]
