_pkgs: _self: super: {

  qc-motoko = super.callCabal2nix "qc-motoko" ../test/random {};
  lsp-int = super.callCabal2nix "lsp-int" ../test/lsp-int {};

  # tests: dist-newstyle: getDirectoryContents:openDirStream: does not exist (No such file or directory)
  # See #2954 for what to do when a custom package should become necessary.
}
