nix: subpath:
  let stdenv = nix.stdenv; in
  self: super: {
  haskell-lsp-types = self.haskell-lsp-types_0_19_0_0;

  haskell-lsp = self.haskell-lsp_0_19_0_0;

  lsp-test = nix.haskell.lib.dontCheck self.lsp-test_0_9_0_0;

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
