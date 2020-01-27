nix: subpath:
  let stdenv = nix.stdenv; in
  self: super: {
  haskell-lsp-types = self.haskell-lsp-types_0_19_0_0;

  haskell-lsp = self.haskell-lsp_0_19_0_0;

  parser-combinators = self.parser-combinators_1_2_1;

  lsp-test = nix.haskell.lib.dontCheck self.lsp-test_0_10_0_0;

  lsp-int = self.callCabal2nix "lsp-int" (subpath "test/lsp-int") { };

  qc-motoko = self.callCabal2nix "qc-motoko" (subpath "test/random") { };

  winter = self.callCabal2nixWithOptions "winter" nix.sources.winter "--no-check" {};

  ic-stub = self.callCabal2nixWithOptions "ic-stub" (subpath "ic-stub") "-frelease" { };
}
