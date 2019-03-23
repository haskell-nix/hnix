{ rpRef ? "ea3c9a1536a987916502701fb6d319a880fdec96" }:

let rp = builtins.fetchTarball "https://github.com/reflex-frp/reflex-platform/archive/${rpRef}.tar.gz";

    hnix-store-src = pkgs: pkgs.fetchFromGitHub {
      owner = "haskell-nix";
      repo = "hnix-store";
      rev = "0.1.0.0";
      sha256 = "1z48msfkiys432rkd00fgimjgspp98dci11kgg3v8ddf4mk1s8g0";
    };

    overlay = pkgs: with pkgs.haskell.lib; self: super:
      let guardGhcjs = p: if self.ghc.isGhcjs or false then null else p;
       in {
         hashing = super.hashing;
         haskeline = guardGhcjs super.haskeline;
         serialise = doJailbreak super.serialise;

         Glob = guardGhcjs super.Glob;
         criterion = guardGhcjs super.criterion;
         pretty-show = guardGhcjs super.pretty;
         repline = guardGhcjs super.repline;
         tasty = guardGhcjs super.tasty;
         tasty-hunit = guardGhcjs super.tasty;
         tasty-th = guardGhcjs super.tasty;
         unix = guardGhcjs super.unix;

         interpolate = self.callCabal2nix "interpolate" (pkgs.fetchFromGitHub {
           owner = "sol";
           repo = "interpolate";
           rev = "2d654444365805458e0310d461b3ecd2826977ff";
           sha256 = "01g88j6qv33r6j4yl6yisr9sk3kcvgp81z6qmhr94ka8z45raii9";
         }) {};
         lens-family-th = self.callCabal2nix "lens-family-th" (pkgs.fetchFromGitHub {
           owner = "DanBurton";
           repo = "lens-family-th";
           rev = "483be4d5b53cc6253e3926623c3aa9334c53debc";
           sha256 = "099dp4f1wwarvhwgnm4nhymnnwlqgrsgrfd8ch6dwmy6myzv2dij";
         }) {};
         megaparsec = dontCheck (self.callCabal2nix "megaparsec" (pkgs.fetchFromGitHub {
           owner = "mrkkrp";
           repo = "megaparsec";
           rev = "7b271a5edc1af59fa435a705349310cfdeaaa7e9";
           sha256 = "0415z18gl8dgms57rxzp870dpz7rcqvy008wrw5r22xw8qq0s13c";
         }) {});
         parser-combinators = self.callCabal2nix "parser-combinators" (pkgs.fetchFromGitHub {
           owner = "mrkkrp";
           repo = "parser-combinators";
           rev = "dd6599224fe7eb224477ef8e9269602fb6b79fe0";
           sha256 = "11cpfzlb6vl0r5i7vbhp147cfxds248fm5xq8pwxk92d1f5g9pxm";
         }) {};
         # Should be callHackage, but it gives an error "found zero or more than one cabal file"
         # unordered-containers = pkgs.haskellPackages.callHackage "unordered-containers" "0.2.9.0" {};
         unordered-containers = self.callCabal2nix "unordered-containers" (pkgs.fetchFromGitHub {
           owner = "tibbe";
           repo = "unordered-containers";
           rev = "0a6b84ec103e28b73458f385ef846a7e2d3ea42f";
           sha256 = "128q8k4py2wr1v0gmyvqvzikk6sksl9aqj0lxzf46763lis8x9my";
         }) {};
       };
in
  (import rp {}).project ({ pkgs, ... }:
  {
    name = "hnix-ghcjs";
    overrides = pkgs.lib.foldr pkgs.lib.composeExtensions (_: _: {})
                [
                 (import "${hnix-store-src pkgs}/overlay.nix")
                 (overlay pkgs)
                ];
    packages = {
      hnix = ../.;
    };
    
    shells = {
      # ghc = [ "hnix" ];
      ghcjs = [ "hnix" ];
    };
  
  })
