{ pkgs ? import <nixpkgs> {}
, ghc ? pkgs.haskell.compiler.ghc984
}:

pkgs.mkShell rec {
  buildInputs = with pkgs; [
    ghc
  ];
}
