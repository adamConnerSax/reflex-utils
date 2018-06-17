{ reflex-platform ? import ./reflex-platform {}, compiler ? "default" }:
#{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let
  localPlatform = import ./packages.nix { reflex-platform = reflex-platform; };
  nixpkgs = reflex-platform.nixpkgs;
  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, bifunctors, bytestring, cabal-macosx
      , clay, containers, data-default, dataBuilder, dependent-map
      , exception-transformers, file-embed, generics-sop, ghc-prim
      , ghcjs-dom, hashable, jsaddle, jsaddle-warp, lens, mmorph
      , monad-control, mtl, perConstructor-sop, pretty-show, process
      , profunctors, readable, ref-tf, reflex, reflex-dom
      , reflex-dom-contrib, reflex-sumType-utilities, safe, semigroups
      , stdenv, template-haskell, text, these, time, transformers, tuple
      , unordered-containers, validation
      }:
      mkDerivation {
        pname = "reflex-utils";
        version = "0.2.1.1";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          base bifunctors bytestring clay containers data-default dataBuilder
          dependent-map exception-transformers file-embed generics-sop
          ghc-prim ghcjs-dom hashable jsaddle lens mmorph monad-control mtl
          perConstructor-sop profunctors readable ref-tf reflex reflex-dom
          reflex-dom-contrib reflex-sumType-utilities safe semigroups
          template-haskell text these time transformers tuple
          unordered-containers validation
        ];
        executableHaskellDepends = [
          base bytestring cabal-macosx clay containers data-default
          dataBuilder exception-transformers file-embed ghcjs-dom hashable
          jsaddle-warp lens mtl pretty-show process profunctors ref-tf reflex
          reflex-dom reflex-dom-contrib safe text time transformers
          unordered-containers validation
        ];
        homepage = "http://github.com/adamConnerSax/reflex-utils#readme";
        description = "Initial project template from stack";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then localPlatform
                       else localPlatform.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
