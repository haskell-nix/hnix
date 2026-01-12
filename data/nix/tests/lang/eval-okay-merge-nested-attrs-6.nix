# Test: real-world pattern from nixpkgs pkg-config-wrapper
{
  env.addFlags = "";
  env = { foo = 1; bar = 2; };
}
