with (import <nixpkgs> {});

haskell.lib.buildStackProject {
  name = "myEnv";
  buildInputs = [ freetype webkitgtk24x darwin.security_tool gcc git fontconfig ];
  SSL_CERT_FILE="${cacert}/etc/ssl/certs/ca-bundle.crt";
  PANGOCAIRO_BACKEND="";
  FONTCONFIG_FILE="${fontconfig}/etc/fonts/fonts.conf";
}