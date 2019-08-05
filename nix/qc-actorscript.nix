{ mkDerivation, base, process, QuickCheck, stdenv, tasty
, tasty-quickcheck, text, turtle
}:
mkDerivation {
  pname = "qc-actorscript";
  version = "0";
  src = ../test/random;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base process QuickCheck tasty tasty-quickcheck text turtle
  ];
  description = "generate numeric tests for ActorScript";
  license = stdenv.lib.licenses.mit;
}
