{ mkDerivation, base, containers, hspec, stdenv, text }:
mkDerivation {
  pname = "envparse";
  version = "0.3.3";
  src = ./.;
  libraryHaskellDepends = [ base containers ];
  testHaskellDepends = [ base containers hspec text ];
  homepage = "https://supki.github.io/envparse";
  description = "Parse environment variables";
  license = stdenv.lib.licenses.bsd3;
}
