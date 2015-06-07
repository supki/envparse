{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc7101" }: let
  inherit (nixpkgs) pkgs;
  ghc = pkgs.haskell.packages.${compiler}.ghcWithPackages( ps: with ps; [
    hdevtools doctest
  ]);
  cabal-install = pkgs.haskell.packages.${compiler}.cabal-install;
  env = (import ./default.nix { inherit nixpkgs compiler; }).env;
in
  pkgs.stdenv.mkDerivation rec {
    name = "envparse";
    buildInputs = [ ghc cabal-install ] ++ env.buildInputs;
    shellHook = "${env.shellHook}\nexport IN_WHICH_NIX_SHELL=${name}";
  }
