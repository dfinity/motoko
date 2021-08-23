# THIS IS AN AUTOMATICALLY GENERATED FILE. DO NOT EDIT MANUALLY!
# See ./nix/generate.nix for instructions.

{ mkDerivation
, pkgs
, base
, exceptions
, lib
, managed
, process
, QuickCheck
, quickcheck-text
, quickcheck-unicode
, tasty
, tasty-quickcheck
, text
, turtle
}:
mkDerivation {
  pname = "qc-motoko";
  version = "1";
  src = import ../gitSource.nix "test/random";
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base
    exceptions
    managed
    process
    QuickCheck
    quickcheck-text
    quickcheck-unicode
    tasty
    tasty-quickcheck
    text
    turtle
  ];
  description = "generate randomised tests for Motoko";
  license = "unknown";
  hydraPlatforms = lib.platforms.none;
}
