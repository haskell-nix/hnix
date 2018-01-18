{ stdenv, fetchFromGitHub }:

stdenv.mkDerivation rec {
  name = "nixos-17.09-${version}";
  version = "2017-01-19";

  # To update the pinned nixpkgs revision:
  #
  # $ nix-prefetch-git git@github.com:NixOS/nixpkgs-channels.git \
  #     --rev refs/heads/nixos-17.09
  #
  # Update the "rev" and "sha256" lines.
  src = fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs-channels";
    rev = "d9a2891c32ee452a2cd701310040b660da0cc853";
    sha256 = "14m6krpv7iga96bjpb4xmdq1fpysryyfvkghn68k6g8gr9y61fqs";
  };

  dontBuild = true;
  preferLocalBuild = true;

  installPhase = ''
    cp -a . $out
  '';
}
