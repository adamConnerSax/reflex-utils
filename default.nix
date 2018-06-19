(import ./reflex-platform {}).project ({ pkgs, ... }:
let nixpkgs = import <nixpkgs> {};
in
{        
  packages = {
    reflex-utils = ./. ; 
  };

  shells = {
   ghc = ["reflex-utils"];
   ghc8_2_1 = ["reflex-utils"];
   #ghcjs = ["common" "frontend"];
  };

  overrides = import ./package-overlay.nix {};

#  shellToolOverrides = ghc: super: {
#  cabal-install = ghc.callCabal2nix "cabal-install" ((nixpkgs.pkgs.fetchFromGitHub {
#    owner = "haskell";
#    repo = "cabal";
#    rev = "d486cfcaa8116bbae62a4cc0294a48c478e2f897";
#    sha256 = "1ddpig5vvz3qyj820spgzvbhd7d056ag1g8kd5w74jnfd3gji958";
#  }) + "/cabal-install") {};
    #cabal-install = ghc.callHackage "cabal-install" "2.2.0.0" {};
#  };

  withHoogle = false;
})
