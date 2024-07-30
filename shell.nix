{ pkgs ? import <nixpkgs> {}
, ghc ? pkgs.haskell.compiler.ghc948
}:

pkgs.mkShell rec {
  buildInputs = with pkgs; [
    ghc
  ];
}
