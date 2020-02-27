# THIS IS AN AUTOMATICALLY GENERATED FILE. DO NOT EDIT MANUALLY!
# See ./nix/generate.nix for instructions.

{ mkDerivation, pkgs, array, base, binary, byte-order, bytestring
, cmdargs, containers, data-default-class, data-fix, deepseq
, directory, filepath, FloatingHex, lifted-base, microlens-platform
, monad-control, mtl, nats, parsec, primitive, primitive-unaligned
, process, stdenv, tasty, tasty-hunit, tasty-quickcheck, temporary
, text, transformers, vector
}:
mkDerivation {
  pname = "winter";
  version = "1.0.0";
  src = pkgs.sources.winter;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    array base binary byte-order bytestring containers
    data-default-class data-fix deepseq FloatingHex lifted-base
    microlens-platform monad-control mtl nats parsec primitive
    primitive-unaligned text transformers vector
  ];
  executableHaskellDepends = [
    base binary bytestring cmdargs containers data-default-class mtl
    parsec text
  ];
  testHaskellDepends = [
    array base binary bytestring containers data-default-class data-fix
    deepseq directory filepath FloatingHex lifted-base
    microlens-platform monad-control mtl parsec primitive process tasty
    tasty-hunit tasty-quickcheck temporary text transformers vector
  ];
  doCheck = false;
  homepage = "https://github.com/dfinity/winter";
  description = "Haskell port of the WebAssembly OCaml reference interpreter";
  license = stdenv.lib.licenses.mit;
}
