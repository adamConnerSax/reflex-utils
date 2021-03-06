{nixpkgs ? import <nixpkgs> {} }:
self: super: {
  validation = super.callHackage "validation" "0.6.0" {}; #fix doctest error
  perConstructor-sop = super.callCabal2nix "perConstructor-sop" (nixpkgs.pkgs.fetchFromGitHub {
    owner = "adamConnerSax";
    repo = "perConstructor-sop";
    rev = "b0c8fd5c4b1576b4ad713821f35d06b0c00ff5f6";
    sha256 = "1lyzzn1bfjgk6p8v62r5r0xqkp6ry4y26f52d3zfix7n1xqfqaq4";
   }) {};
   dataBuilder = super.callCabal2nix "dataBuilder" (nixpkgs.pkgs.fetchFromGitHub {  
     owner = "adamConnerSax";
     repo = "dataBuilder";
     rev = "e6a84e66bcb4afbd7ea7a1035b06c722b4efa61";
     sha256 = "1lcxy5gdgkmviqz84ar88wrvxj5nrncir1mqx56pdb8rcz0s90kq";
   }) {};
   reflex-sumType-utilities = super.callCabal2nix "reflex-sumType-utilities" (nixpkgs.pkgs.fetchFromGitHub {
     owner = "adamConnerSax";
     repo = "reflex-sumType-utilities";
     rev = "a0a73c80df4c4265223ca9ea38a8ceaed8b3ad52";
     sha256 = "03rnll7m8vqq1wspc8kmxdccjaclamywxaggn51xbwpzj4r83giy";
   }) {};
   reflex-collections = super.callCabal2nix "reflex-collections" (nixpkgs.pkgs.fetchFromGitHub {
     owner = "adamConnerSax";
     repo = "reflex-collections";
     rev = "4be6ab91775ecdedf954c484fc897b30564fd56e";
     sha256 = "0rc0vpk0fij4a7kqr1js35zjbqpbn8gzxhf24l8p1r6djj97k473";
   }) {};
#   reflex-dom-contrib = super.callCabal2nix "reflex-dom-contrib" (nixpkgs.pkgs.fetchFromGitHub {
#     owner = "reflex-frp";
#     repo = "reflex-dom-contrib";
#     rev = "5b5da107ccf7b4e1544dc6f6bdb6b6de7525a04d";
#     sha256 = "0lmddfw7vhvc2yf22jdqsnr3aqz2j1k96v53ik1mr6kxl6265yms";
#   }) {};
   reflex-dom-contrib = super.callCabal2nix "reflex-dom-contrib" (nixpkgs.pkgs.fetchFromGitHub {
     owner = "adamConnerSax";
     repo = "reflex-dom-contrib";
     rev = "0eed765f1945d76293238cca32f83d00fb04c61d";
     sha256 = "0b0zlzbhd7c1syggn196jhhm67s4nsddmyd29njjnk537lw1d2qv";
   }) {};
}      
