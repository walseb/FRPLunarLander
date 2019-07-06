{ mkDerivation, base, gloss, reactive-banana, stdenv }:
mkDerivation {
  pname = "Project9";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base gloss reactive-banana ];
  license = stdenv.lib.licenses.bsd3;
}
