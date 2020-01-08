nix: subpath:
  let stdenv = nix.stdenv; in
  self: super: {
  haskell-lsp-types = self.haskell-lsp-types_0_19_0_0;

  haskell-lsp = self.haskell-lsp_0_19_0_0;

  lsp-test = self.callPackage
    ({ mkDerivation, aeson, aeson-pretty, ansi-terminal, async, base
     , bytestring, conduit, conduit-parse, containers, data-default
     , Diff, directory, filepath, hspec, haskell-lsp, lens, mtl
     , parser-combinators, process, rope-utf16-splay, text, transformers
     , unix, unordered-containers
     }:
       mkDerivation {
         pname = "lsp-test";
         version = "0.9.0.0";
         sha256 = "0igd27msf3ya4i3pby434d0pa51qpr27vxyfv0q4i38ajj4ndsx4";
         libraryHaskellDepends = [
           aeson aeson-pretty ansi-terminal async base bytestring conduit
           conduit-parse containers data-default Diff directory filepath
           haskell-lsp lens mtl parser-combinators process rope-utf16-splay
           text transformers unix unordered-containers
         ];
         doCheck = false; # crucial
         testHaskellDepends = [
           aeson base data-default haskell-lsp hspec lens text
           unordered-containers
         ];
         description = "Functional test framework for LSP servers";
         license = stdenv.lib.licenses.bsd3;
         hydraPlatforms = stdenv.lib.platforms.none;
       }) {};
  # lsp-test = self.lsp-test_0_9_0_0;

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
