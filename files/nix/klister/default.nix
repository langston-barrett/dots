{ mkDerivation, base, bifunctors, bytestring, call-stack
, containers, deepseq, directory, extra, filepath, hedgehog, hpack
, lens, megaparsec, mtl, optparse-applicative, prettyprinter
, stdenv, tasty, tasty-golden, tasty-hedgehog, tasty-hunit, text
, transformers
}:
mkDerivation {
  pname = "klister";
  version = "0.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bifunctors containers directory extra filepath lens megaparsec
    mtl prettyprinter text
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    base bifunctors containers directory extra filepath lens megaparsec
    mtl optparse-applicative prettyprinter text
  ];
  testHaskellDepends = [
    base bifunctors bytestring call-stack containers deepseq directory
    extra filepath hedgehog lens megaparsec mtl prettyprinter tasty
    tasty-golden tasty-hedgehog tasty-hunit text transformers
  ];
  prePatch = "hpack";
  homepage = "https://github.com/gelisam/klister#readme";
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
