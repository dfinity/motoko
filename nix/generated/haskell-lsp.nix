{ mkDerivation
, aeson
, async
, attoparsec
, base
, bytestring
, containers
, data-default
, directory
, filepath
, hashable
, haskell-lsp-types
, hslogger
, hspec
, hspec-discover
, lens
, lib
, mtl
, network-uri
, QuickCheck
, quickcheck-instances
, rope-utf16-splay
, sorted-list
, stm
, temporary
, text
, time
, unordered-containers
}:
mkDerivation {
  pname = "haskell-lsp";
  version = "0.23.0.0";
  sha256 = "b087f0ee6b1d98ce2188e71208aba23284b29d733c7295bd01afabb731767e1f";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson
    async
    attoparsec
    base
    bytestring
    containers
    data-default
    directory
    filepath
    hashable
    haskell-lsp-types
    hslogger
    lens
    mtl
    network-uri
    rope-utf16-splay
    sorted-list
    stm
    temporary
    text
    time
    unordered-containers
  ];
  testHaskellDepends = [
    aeson
    base
    bytestring
    containers
    data-default
    directory
    filepath
    hashable
    hspec
    lens
    network-uri
    QuickCheck
    quickcheck-instances
    rope-utf16-splay
    sorted-list
    stm
    text
    unordered-containers
  ];
  testToolDepends = [ hspec-discover ];
  homepage = "https://github.com/alanz/haskell-lsp";
  description = "Haskell library for the Microsoft Language Server Protocol";
  license = lib.licenses.mit;
}
