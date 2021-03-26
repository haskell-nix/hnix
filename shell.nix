attrs@{...}:
let defaultAttrs = {
  # Defaults are put in this form deliberately. Details: #748
  withHoogle = true;
  compiler = "ghc8104";
};
in (import ./. (defaultAttrs // attrs)).env
