{ mkDerivation
, aeson
, base
, binary
, bytestring
, data-default
, deepseq
, filepath
, hashable
, lens
, lib
, network-uri
, scientific
, text
, unordered-containers
}:
mkDerivation {
  pname = "haskell-lsp-types";
  version = "0.23.0.0";
  sha256 = "07afebcfd9e99dcc87a5c00b1af3ec0649caf41a7eaa248475700664004ae037";
  libraryHaskellDepends = [
    aeson
    base
    binary
    bytestring
    data-default
    deepseq
    filepath
    hashable
    lens
    network-uri
    scientific
    text
    unordered-containers
  ];
  homepage = "https://github.com/alanz/haskell-lsp";
  description = "Haskell library for the Microsoft Language Server Protocol, data types";
  license = lib.licenses.mit;
}
