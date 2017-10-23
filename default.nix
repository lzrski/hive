with import <nixpkgs> {}; stdenv.mkDerivation {
  name = "node";
  buildInputs = [ nodejs-8_x ];
  shellHook = ''
    # Start user's preffered shell
    exec ${builtins.getEnv "SHELL"}
    '';
}
