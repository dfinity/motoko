nix: subpath:
  let stdenv = nix.stdenv; in
  self: super: {

  qc-motoko = super.callPackage generated/random.nix {};
  lsp-int = super.callPackage generated/lsp-int.nix {};

  # tests: dist-newstyle: getDirectoryContents:openDirStream: does not exist (No such file or directory)
  lsp-test = nix.haskell.lib.dontCheck (super.callPackage generated/lsp-test.nix {});
}
