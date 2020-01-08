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
      owner = "dfinity-side-projects";
      repo = "winter";
      rev = "1e16b471644be26160cc20c4e2144c643c547ed8";
      sha256 = "191fk1dv8vp28rwpl75nz9y3pbypc8jv9w669mvl9d2k5f74jirm";
     }) "--no-check" {};

  ic-stub = self.callCabal2nixWithOptions "ic-stub" (subpath "ic-stub") "-frelease" { };
}
