({ x ? 1, y ? x * 3 }: import ./file2.nix { a = y; }) {}
