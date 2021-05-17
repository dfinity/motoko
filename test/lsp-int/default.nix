{ mkDerivation, base, data-default, directory, filepath
, haskell-lsp-types, hspec, HUnit, lens, lsp-test, stdenv, text
}:
mkDerivation {
  pname = "lsp-int";
  version = "0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base data-default directory filepath haskell-lsp-types hspec HUnit
    lens lsp-test text
  ];
  description = "Integration tests for the language server";
  license = "unknown";
}
