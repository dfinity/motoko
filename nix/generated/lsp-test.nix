{ mkDerivation
, aeson
, aeson-pretty
, ansi-terminal
, async
, base
, bytestring
, conduit
, conduit-parse
, containers
, data-default
, Diff
, directory
, filepath
, Glob
, haskell-lsp
, hspec
, lens
, lib
, mtl
, parser-combinators
, process
, text
, transformers
, unix
, unordered-containers
}:
mkDerivation {
  pname = "lsp-test";
  version = "0.11.0.6";
  sha256 = "7ae5ec14c7331c0ee12e0d42ee6c3f1bf97c686b548e4c870acbf02c50896238";
  revision = "1";
  editedCabalFile = "0bmi9invhshqq9b0nrbn8hgawgxhgyxw9li4rb9ls2jknlhnafhd";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson
    aeson-pretty
    ansi-terminal
    async
    base
    bytestring
    conduit
    conduit-parse
    containers
    data-default
    Diff
    directory
    filepath
    Glob
    haskell-lsp
    lens
    mtl
    parser-combinators
    process
    text
    transformers
    unix
    unordered-containers
  ];
  testHaskellDepends = [
    aeson
    base
    data-default
    directory
    filepath
    haskell-lsp
    hspec
    lens
    text
    unordered-containers
  ];
  homepage = "https://github.com/bubba/lsp-test#readme";
  description = "Functional test framework for LSP servers";
  license = lib.licenses.bsd3;
}
