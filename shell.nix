attrs@{...}:
let defaultAttrs = {
  # Defaults are put in this form deliberately. Details: #748
  withHoogle = true;
  compiler = "ghc8107";
};
in (import ./. (defaultAttrs // attrs)).env
