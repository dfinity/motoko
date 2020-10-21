# THIS IS AN AUTOMATICALLY GENERATED FILE. DO NOT EDIT MANUALLY!
# See ./nix/generate.nix for instructions.

{ mkDerivation
, pkgs
, base
, data-default
, directory
, filepath
, haskell-lsp-types
, hspec
, HUnit
, lens
, lsp-test
, stdenv
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
    haskell-lsp-types
    hspec
    HUnit
    lens
    lsp-test
    text
  ];
  description = "Integration tests for the language server";
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
