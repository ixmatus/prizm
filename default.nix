{ mkDerivation, base, convertible, HUnit, mono-traversable
, QuickCheck, stdenv, test-framework, test-framework-hunit
, test-framework-quickcheck2, text
}:
mkDerivation {
  pname = "prizm";
  version = "2.0.0";
  src = ./.;
  libraryHaskellDepends = [ base convertible mono-traversable text ];
  testHaskellDepends = [
    base convertible HUnit mono-traversable QuickCheck test-framework
    test-framework-hunit test-framework-quickcheck2
  ];
  homepage = "https://github.com/ixmatus/prizm";
  description = "Convert colors to different color spaces, interpolate colors, and transform colors";
  license = stdenv.lib.licenses.bsd3;
}
