nix: subpath:
  let stdenv = nix.stdenv; in
  self: super: {

  lsp-int = super.callPackage generated/lsp-int.nix {};

  qc-motoko = super.callPackage generated/random.nix {};
}
