{ cabal, hspec }:

cabal.mkDerivation (self: {
  pname = "envparse";
  version = "0.1.0";
  src = ./.;
  testDepends = [ hspec ];
  meta = {
    homepage = "http://example.com/";
    description = "Parse environment variables";
    license = self.stdenv.lib.licenses.bsd3; # this is a lie
    platforms = self.ghc.meta.platforms;
  };
})
