(import ./reflex-platform {}).project ({ pkgs, ... }: {
  packages = {
    reflex-utils = ./. ; 
  };

  shells = {
   ghc = ["reflex-utils"];
   #ghcjs = ["common" "frontend"];
  };

  overrides = import ./package-overlay.nix {};
  withHoogle = false;
})
