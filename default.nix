{ mkDerivation, pkgs, ghc, base, bytestring, clay, containers, dataBuilder
, file-embed, ghc-prim, ghcjs-dom, hashable, lens
, mmorph, mtl, pretty-show, readable, reflex, reflex-dom
, reflex-dom-contrib, semigroups, stdenv, template-haskell, text
, time, transformers, unordered-containers
, ghcjs-base ? null
}:
let inherit (pkgs.stdenv.lib) optional;
in mkDerivation {
  pname = "reflex-utils";
  version = "0.2.1.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring clay containers dataBuilder ghc-prim
    ghcjs-dom hashable lens mmorph mtl readable reflex reflex-dom
    reflex-dom-contrib semigroups template-haskell text time
    transformers unordered-containers
  ] ++ optional (ghc.isGhcjs or false) ghcjs-base;
  executableHaskellDepends = [
    base bytestring clay containers file-embed hashable mtl pretty-show
    reflex reflex-dom reflex-dom-contrib text time transformers
    unordered-containers
  ];
  homepage = "http://github.com/adamConnerSax/reflex-utils#readme";
  description = "Initial project template from stack";
  license = stdenv.lib.licenses.bsd3;
}
