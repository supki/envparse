{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc7101" }: let
  inherit (nixpkgs) pkgs;
  ghc = pkgs.haskell.packages.${compiler}.ghcWithPackages( ps: with ps; [
    hdevtools doctest
  ]);
  env = (import ./default.nix { inherit nixpkgs compiler; }).env;
in
  pkgs.stdenv.mkDerivation {
    name = "envparse";
    buildInputs = [ ghc ] ++ env.buildInputs;
    shellHook = env.shellHook;
  }
