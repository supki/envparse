{ mkDerivation, base, containers, hspec, stdenv }:
mkDerivation {
  pname = "envparse";
  version = "0.2.1";
  src = ./.;
  buildDepends = [ base containers ];
  testDepends = [ base containers hspec ];
  homepage = "https://supki.github.io/envparse";
  description = "Parse environment variables";
  license = stdenv.lib.licenses.bsd2;
}
