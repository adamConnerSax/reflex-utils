(import ./reflex-platform {}).project ({ pkgs, ... }: {        
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
#    cabal-install = ghc.callHackage "cabal-install" "2.2.0.0" {};
#  };

  withHoogle = false;
})
