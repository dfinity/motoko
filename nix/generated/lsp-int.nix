# THIS IS AN AUTOMATICALLY GENERATED FILE. DO NOT EDIT MANUALLY!
# See ./nix/generate.nix for instructions.

{ mkDerivation
, pkgs
, base
, data-default
, directory
, filepath
, hspec
, HUnit
, lens
, lib
, lsp-test
, lsp-types
, text
}:
mkDerivation {
  pname = "lsp-int";
  version = "0";
  src = import ../gitSource.nix "test/lsp-int";
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base
    data-default
    directory
    filepath
    hspec
    HUnit
    lens
    lsp-test
    lsp-types
    text
  ];
  description = "Integration tests for the language server";
  license = "unknown";
  mainProgram = "lsp-int";
}
