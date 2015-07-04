{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc7101" }: let
  inherit (nixpkgs) pkgs;
  ghc = pkgs.haskell.packages.${compiler}.ghcWithPackages(ps: [
    ps.hdevtools ps.doctest ps.hspec-discover
  ]);
  cabal-install = pkgs.haskell.packages.${compiler}.cabal-install;
  pkg = (import ./default.nix { inherit nixpkgs compiler; });
in
  pkgs.stdenv.mkDerivation rec {
    name = pkg.pname;
    buildInputs = [ ghc cabal-install ] ++ pkg.env.buildInputs;
    shellHook = ''
      ${pkg.env.shellHook}
      export IN_WHICH_NIX_SHELL=${name}
      cabal --no-require-sandbox configure --package-db=$NIX_GHC_LIBDIR/package.conf.d --enable-tests
    '';
  }
