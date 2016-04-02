with (import <nixpkgs> {});

haskell.lib.buildStackProject {
  name = "myEnv";
  buildInputs = [ webkitgtk24x darwin.security_tool gcc git ];
  SSL_CERT_FILE="${cacert}/etc/ssl/certs/ca-bundle.crt";
}