# Test: nested path + direct attrset merge
# This pattern is used in nixpkgs (e.g., pkg-config-wrapper)
{ a.x = 1; a = { y = 2; }; }
