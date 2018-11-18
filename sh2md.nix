{ mkDerivation, base, containers, Hclip, hpack, hspec
, optparse-applicative, process, stdenv, text, transformers, unix
}:
mkDerivation {
  pname = "sh2md";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base containers Hclip optparse-applicative process text
    transformers unix
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    base containers Hclip optparse-applicative process text
    transformers unix
  ];
  testHaskellDepends = [
    base containers Hclip hspec optparse-applicative process text
    transformers unix
  ];
  preConfigure = "hpack";
  homepage = "https://github.com/kkweon/sh2md#readme";
  description = "Record your shell session and print in the markdown format";
  license = stdenv.lib.licenses.bsd3;
}
