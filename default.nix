{ mkDerivation, base, convertible, data-default-class
, mono-traversable, QuickCheck, stdenv, test-framework
, test-framework-hunit, test-framework-quickcheck2, text
}:
mkDerivation {
  pname = "prizm";
  version = "1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base convertible data-default-class mono-traversable text
  ];
  testHaskellDepends = [
    base convertible mono-traversable QuickCheck test-framework
    test-framework-hunit test-framework-quickcheck2
  ];
  homepage = "https://github.com/ixmatus/prizm";
  description = "Compute with colors and differenct color spaces";
  license = stdenv.lib.licenses.bsd3;
}
