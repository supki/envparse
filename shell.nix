{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc802" }: let
  inherit (nixpkgs) pkgs;
  haskell = pkgs.haskell.packages.${compiler};

  ghc = haskell.ghcWithPackages(ps: [
    ps.hdevtools ps.doctest ps.hspec-discover ps.hlint ps.ghc-mod
  ]);

  this = (import ./default.nix { inherit nixpkgs compiler; });
in
  pkgs.stdenv.mkDerivation rec {
    name = this.pname;
    buildInputs = [
      ghc
      haskell.cabal-install

      pkgs.moreutils
    ] ++ this.env.buildInputs;
    shellHook = ''
      ${this.env.shellHook}
      chronic cabal configure --package-db=$NIX_GHC_LIBDIR/package.conf.d --enable-tests
    '';
  }
