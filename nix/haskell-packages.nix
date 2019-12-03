nix: subpath:
  let stdenv = nix.stdenv; in
  self: super: {
  haskell-lsp-types = self.callPackage
    ({ mkDerivation, aeson, base, bytestring, data-default, deepseq
     , filepath, hashable, lens, network-uri, scientific, text
     , unordered-containers
     }:
       mkDerivation {
         pname = "haskell-lsp-types";
         version = "0.18.0.0";
         sha256 = "1ypa3gxxcg7dl905d4nprcqzw2fcl7z7xy1hpg5har1dw3w9fyiq";
         libraryHaskellDepends = [
           aeson base bytestring data-default deepseq filepath hashable lens
           network-uri scientific text unordered-containers
         ];
         description = "Haskell library for the Microsoft Language Server Protocol, data types";
         license = stdenv.lib.licenses.mit;
         hydraPlatforms = stdenv.lib.platforms.none;
       }) {};

  haskell-lsp = self.callPackage
    ({ mkDerivation, aeson, async, attoparsec, base, bytestring
     , containers, data-default, directory, filepath, hashable
     , haskell-lsp-types, hslogger, hspec, hspec-discover, lens, mtl
     , network-uri, QuickCheck, quickcheck-instances, rope-utf16-splay
     , sorted-list, stm, temporary, text, time, unordered-containers
     }:
       mkDerivation {
         pname = "haskell-lsp";
         version = "0.18.0.0";
         sha256 = "0q9xpjgr3n7svhd9f7mmkw113avswvfm08fnbijdr1sblipd12sl";
         isLibrary = true;
         isExecutable = true;
         libraryHaskellDepends = [
           aeson async attoparsec base bytestring containers data-default
           directory filepath hashable haskell-lsp-types hslogger lens mtl
           network-uri rope-utf16-splay sorted-list stm temporary text time
           unordered-containers
         ];
         testHaskellDepends = [
           aeson base bytestring containers data-default directory filepath
           hashable hspec lens network-uri QuickCheck quickcheck-instances
           rope-utf16-splay sorted-list stm text
         ];
         testToolDepends = [ hspec-discover ];
         description = "Haskell library for the Microsoft Language Server Protocol";
         license = stdenv.lib.licenses.mit;
         hydraPlatforms = stdenv.lib.platforms.none;
       }) {};

  lsp-test = self.callPackage
    ({ mkDerivation, aeson, aeson-pretty, ansi-terminal, async, base
     , bytestring, conduit, conduit-parse, containers, data-default
     , Diff, directory, filepath, hspec, haskell-lsp, lens, mtl
     , parser-combinators, process, rope-utf16-splay, text, transformers
     , unix, unordered-containers
     }:
       mkDerivation {
         pname = "lsp-test";
         version = "0.8.2.0";
         sha256 = "1762avrs4fkkd9v8i76giaf6km5f3lds5laickn7la7zq7ysh8a3";
         libraryHaskellDepends = [
           aeson aeson-pretty ansi-terminal async base bytestring conduit
           conduit-parse containers data-default Diff directory filepath
           haskell-lsp lens mtl parser-combinators process rope-utf16-splay
           text transformers unix unordered-containers
         ];
         doCheck = false;
         testHaskellDepends = [
           aeson base data-default haskell-lsp hspec lens text
           unordered-containers
         ];
         description = "Functional test framework for LSP servers";
         license = stdenv.lib.licenses.bsd3;
         hydraPlatforms = stdenv.lib.platforms.none;
       }) {};

  lsp-int = self.callCabal2nix "lsp-int" (subpath "test/lsp-int") { };

  qc-motoko = self.callCabal2nix "qc-motoko" (subpath "test/random") { };

  winter = self.callCabal2nixWithOptions "winter"
    (nix.fetchFromGitHub {
      owner = "dfinity";
      repo = "winter";
      rev = "4295ff98da8ca890e824130152a78892ad6420ba";
      sha256 = "05wr3066mlz7hh2s49wgf9pgdsh1bsivnhp6j7hklmw2cnj9g0sl";
     }) "--no-check" {};

  ic-stub = self.callCabal2nixWithOptions "ic-stub" (subpath "ic-stub") "-frelease" { };
}
