all: default.nix shell.nix

default.nix: hnix.cabal
	cabal2nix . > $@

shell.nix: hnix.cabal
	cabal2nix --shell . > $@
