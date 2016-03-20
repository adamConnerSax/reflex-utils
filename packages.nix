{ reflex-platform, ... }: reflex-platform.ghcjs.override {
  overrides = self: super: {
    dataBuilder = self.callPackage (reflex-platform.cabal2nixResult (reflex-platform.nixpkgs.fetchgit (builtins.fromJSON ''
      {
        "url": "https://github.com/adamConnerSax/dataBuilder",
        "rev": "507a6d0c1679fb21428fdb188c9d929d622be951",
        "sha256": "10mq48djchhrdxh3b78c5yyz3dywqy96nv7nzw7yplikib2gxydj"
      }
    ''))) {};
  };
}
