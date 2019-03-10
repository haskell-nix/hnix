with builtins;

[ (getContext "foo")
  (attrValues (getContext (toFile "foo" "foo contents")))
  # TODO: Re-enable this once output hash is correct.
  # (getContext (toFile "foo" "foo contents"))
]
