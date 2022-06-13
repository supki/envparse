{ pkgs ? import <nixpkgs> {}
, ghc ? pkgs.haskell.compiler.ghc8107
}:

pkgs.mkShell rec {
  buildInputs = with pkgs; [
    ghc
  ];
}
