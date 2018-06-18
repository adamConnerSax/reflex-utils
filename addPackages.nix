{ reflex-platform ? import ./reflex-platform {}, haskellPackagesPlus ? import ./packages.nix {reflex-platform = reflex-platform;} }:

let
  nixpkgs = reflex-platform.nixpkgs // { pkgs = reflex-platform.nixpkgs.pkgs // { haskellPackages = haskellPackagesPlus; haskell = haskellPackagesPlus; } ; }; 
in
  nixpkgs

