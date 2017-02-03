{ reflex-platform, ... }: reflex-platform.ghc.override {
  overrides = self: super: {
    dataBuilder = self.callPackage (reflex-platform.cabal2nixResult (reflex-platform.nixpkgs.fetchgit (builtins.fromJSON ''
      {
        "url": "https://github.com/adamConnerSax/dataBuilder",
        "rev": "18f65388a3cad7e3687cc59e112402767a1942bd",
        "sha256": "0kymfw47zwkqgd3z3v4al8c27w9ngvjhdyndjnab8b8pi999kwjf"
      }
    ''))) {};
    reflex-dom-contrib = self.callPackage (reflex-platform.cabal2nixResult (reflex-platform.nixpkgs.fetchgit (builtins.fromJSON ''
      {
        "url": "https://github.com/reflex-frp/reflex-dom-contrib",
        "rev": "41c67daf2ad74281f2488cb80ceab7d12292142b",
        "sha256": "1hawxpnx29yiakqrv444153i9hh9844rvcdc151df9pnlkz85gp0"
      }
    ''))) {};
  };
}
