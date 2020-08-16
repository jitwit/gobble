{ mkDerivation, base, binary, criterion, deepseq, HUnit, mtl
, QuickCheck, stdenv, tasty, tasty-hunit, tasty-quickcheck
, unordered-containers, vector, vector-binary-instances
}:
mkDerivation {
  pname = "packed-dawg";
  version = "0.2.0.8";
  src = ./.;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    base binary deepseq mtl unordered-containers vector
    vector-binary-instances
  ];
  testHaskellDepends = [
    base binary deepseq HUnit mtl QuickCheck tasty tasty-hunit
    tasty-quickcheck unordered-containers vector
    vector-binary-instances
  ];
  benchmarkHaskellDepends = [
    base binary criterion deepseq mtl unordered-containers vector
    vector-binary-instances
  ];
  description = "Generation and traversal of highly compressed directed acyclic word graphs";
  license = stdenv.lib.licenses.bsd3;
}
