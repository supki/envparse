{ haskellPackages ? (import <nixpkgs> {}).haskellPackages }:

haskellPackages.cabal.mkDerivation (self: {
  pname = "envparse";
  version = "0.1.0";
  src = builtins.filterSource (path: type: type != "unknown") ./.;
  testDepends = with haskellPackages; [ hspec ];
  meta = {
    homepage = "https://github.com/supki/envparse";
    description = "Parse environment variables";
    license = self.stdenv.lib.licenses.bsd2;
    platforms = self.ghc.meta.platforms;
  };
})
