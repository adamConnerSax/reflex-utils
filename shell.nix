with (import <nixpkgs> {});

haskell.lib.buildStackProject {
  name = "myEnv";
  buildInputs = [ webkitgtk24x darwin.security_tool gcc ];
}