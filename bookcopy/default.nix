{ mkDerivation, base, hpack, stdenv, text-metrics, turtle }:

mkDerivation {
  pname = "bookcopy";
  version = "0.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [ base text-metrics turtle ];
  preConfigure = "hpack";
  license = stdenv.lib.licenses.bsd3;
}
