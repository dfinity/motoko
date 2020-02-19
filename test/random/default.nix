# THIS IS AN AUTOMATICALLY GENERATED FILE. DO NOT EDIT MANUALLY!
# To regenerate this file execute the following command in this directory:
#
# cp $(nix-build ./generate.nix --no-link)/default.nix ./default.nix
{ mkDerivation, base, exceptions, managed, process, QuickCheck
, quickcheck-text, quickcheck-unicode, stdenv, tasty
, tasty-quickcheck, text, turtle
}:
mkDerivation {
  pname = "qc-motoko";
  version = "1";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base exceptions managed process QuickCheck quickcheck-text
    quickcheck-unicode tasty tasty-quickcheck text turtle
  ];
  description = "generate randomised tests for Motoko";
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
