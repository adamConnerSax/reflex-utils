with (import <nixpkgs> {});

haskell.lib.buildStackProject {
  name = "myEnv";
  buildInputs = [ webkitgtk24x darwin.security_tool gcc49 git ];
  GIT_SSL_CAINFO="${cacert}/etc/ssl/certs/ca-bundle.crt";
  SSL_CERT_FILE="${cacert}/etc/ssl/certs/ca-bundle.crt";
  PANGOCAIRO_BACKEND="";
  FONTCONFIG_FILE="${fontconfig}/etc/fonts/fonts.conf";
}