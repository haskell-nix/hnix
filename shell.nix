attrs@{...}:
let defaultAttrs = {
  withHoogle = true;
  compiler = "ghc8101";
};
in (import ./. (defaultAttrs // attrs)).env
