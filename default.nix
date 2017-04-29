{ mkDerivation, base, convertible, HUnit, QuickCheck, stdenv
, test-framework, test-framework-hunit, test-framework-quickcheck2
, text
}:
mkDerivation {
  pname = "prizm";
  version = "3.0.0";
  src = ./.;
  libraryHaskellDepends = [ base convertible text ];
  testHaskellDepends = [
    base convertible HUnit QuickCheck test-framework
    test-framework-hunit test-framework-quickcheck2
  ];
  homepage = "https://github.com/ixmatus/prizm";
  description = "Convert colors to different color spaces, interpolate colors, and transform colors";
  license = stdenv.lib.licenses.bsd3;
}
