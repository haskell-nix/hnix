all: default.nix

default.nix: hnix.cabal
	cabal2nix --shell . > $@
