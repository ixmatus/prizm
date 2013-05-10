all: build test clean

build:
	ghc -package test-framework -package test-framework-quickcheck -package test-framework-hunit -threaded QC.hs -o RunTests

test:
	./RunTests --maximum-generated-tests=1000 +RTS -N2

clean:
	find ./ -name "*.hi" -delete && find ./ -name "*.o" -delete && rm -f RunTests
