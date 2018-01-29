{ mkDerivation, base, language-c, stdenv }:
mkDerivation {
  pname = "c-dsl";
  version = "0.3.1";
  src = ./.;
  libraryHaskellDepends = [ base language-c ];
  description = "A higher level DSL on top of language-c";
  license = stdenv.lib.licenses.mit;
}
