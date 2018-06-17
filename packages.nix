{ reflex-platform, ... }: reflex-platform.ghc.override {
  overrides = self: super: {
    perConstructor-sop = self.callPackage (reflex-platform.cabal2nixResult (reflex-platform.nixpkgs.fetchgit (builtins.fromJSON ''
      {
        "url": "https://github.com/adamConnerSax/perConstructor-sop",
        "rev": "b0c8fd5c4b1576b4ad713821f35d06b0c00ff5f6",
        "sha256": "1lyzzn1bfjgk6p8v62r5r0xqkp6ry4y26f52d3zfix7n1xqfqaq4"
      }   
    ''))) {};   
    dataBuilder = self.callPackage (reflex-platform.cabal2nixResult (reflex-platform.nixpkgs.fetchgit (builtins.fromJSON ''
      {
        "url": "https://github.com/adamConnerSax/dataBuilder",
        "rev": "e6a84e66bcb4afbd7ea7a1035b06c722b4efa61",
        "sha256": "1lcxy5gdgkmviqz84ar88wrvxj5nrncir1mqx56pdb8rcz0s90kq"
      }
    ''))) {};
    reflex-sumType-utilities = self.callPackage (reflex-platform.cabal2nixResult (reflex-platform.nixpkgs.fetchgit (builtins.fromJSON ''
    {
      "url": "https://github.com/adamConnerSax/reflex-sumType-utilities",
      "rev": "a0a73c80df4c4265223ca9ea38a8ceaed8b3ad52",
      "sha256": "03rnll7m8vqq1wspc8kmxdccjaclamywxaggn51xbwpzj4r83giy" 
    }
    ''))) {};
    reflex-dom-contrib = self.callPackage (reflex-platform.cabal2nixResult (reflex-platform.nixpkgs.fetchgit (builtins.fromJSON ''
      {
        "url": "https://github.com/reflex-frp/reflex-dom-contrib",
        "rev": "11a4965f0d124f40ac278e01a73be2e3c5c99161",
        "sha256": "1hawxpnx29yiakqrv444153i9hh9844rvcdc151df9pnlkz85gp0"
      }
    ''))) {};
  };
}
