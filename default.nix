with import <nixpkgs> {}; stdenv.mkDerivation {
  name = "node";
  buildInputs = [
    nodejs-8_x
    elmPackages.elm
    elmPackages.elm-format
  ];
  shellHook = ''
    # Start user's preferred shell
    exec ${builtins.getEnv "SHELL"}
  '';
}
