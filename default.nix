with import <nixpkgs> {};
let
  src-monad-control-identity = fetchGit {
    url = "https://github.com/jumper149/monad-control-identity.git";
    ref = "master";
    rev = "15949d07dedbf95ad6536ac84be4d2f18dfa9523";
  };
  ghc = pkgs.haskell.packages.ghc884.override {
    overrides = self: super: {
      monad-control-identity = self.callCabal2nix "monad-control-identity" "${src-monad-control-identity}" {};
    };
  };
in
  ghc.callCabal2nix "wai-control" ./. {}
