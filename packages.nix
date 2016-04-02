{ reflex-platform, ... }: reflex-platform.ghcjs.override {
  overrides = self: super: {
    dataBuilder = self.callPackage (reflex-platform.cabal2nixResult (reflex-platform.nixpkgs.fetchgit (builtins.fromJSON ''
      {
        "url": "https://github.com/adamConnerSax/dataBuilder",
        "rev": "18f65388a3cad7e3687cc59e112402767a1942bd",
        "sha256": "0kymfw47zwkqgd3z3v4al8c27w9ngvjhdyndjnab8b8pi999kwjf"
      }
    ''))) {};
  };
}
