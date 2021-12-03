nix: subpath:
  let stdenv = nix.stdenv; in
  self: super: {

  qc-motoko = super.callPackage generated/random.nix {};
  lsp-int = super.callPackage generated/lsp-int.nix {};

  # tests: dist-newstyle: getDirectoryContents:openDirStream: does not exist (No such file or directory)
  # See #2954 for what to do when a custom package should become necessary.
}
