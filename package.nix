{ mkDerivation, base, containers, hspec, stdenv }:
mkDerivation {
  pname = "envparse";
  version = "0.3.0";
  src = ./.;
  libraryHaskellDepends = [ base containers ];
  testHaskellDepends = [ base containers hspec ];
  homepage = "https://supki.github.io/envparse";
  description = "Parse environment variables";
  license = stdenv.lib.licenses.bsd3;
}
