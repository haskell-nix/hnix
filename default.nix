{ pkgs ? import <nixpkgs> {} }: pkgs.haskellPackages.developPackage {
    root = ./.;
    source-overrides = {
      unordered-containers = "0.2.9.0";
      # Use a particular commit from github
      insert-ordered-containers = pkgs.fetchFromGitHub {
        owner = "mightybyte";
        repo = "insert-ordered-containers";
        rev = "87054c519b969b62131bcf7a183470d422cbb535";
        sha256 = "0l0g6ns5bcrcaij0wbdgc04qyl9h0vk1kx9lkzdkwj9v51l26azm";
      };
    };
}
