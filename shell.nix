{ pkgs ? import <nixpkgs> {}
, ghc ? pkgs.haskell.compiler.ghc984
}:

pkgs.mkShell rec {
  buildInputs = with pkgs; [
    ghc
    zlib
  ];

  shellHook = ''
    chmod go-w . .ghci
    export LD_LIBRARY_PATH="${pkgs.lib.makeLibraryPath buildInputs}"
  '';
}
