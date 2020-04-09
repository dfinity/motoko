nix: subpath:
  let stdenv = nix.stdenv; in
  self: super: {
  haskell-lsp-types = self.haskell-lsp-types_0_20_0_0;

  haskell-lsp = self.haskell-lsp_0_20_0_1;

  parser-combinators = self.parser-combinators_1_2_1;

  lsp-test = nix.haskell.lib.dontCheck self.lsp-test_0_10_1_0;

  lsp-int = super.callPackage generated/lsp-int.nix {};

  qc-motoko = super.callPackage generated/random.nix {};
}
