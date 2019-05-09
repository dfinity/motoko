let overlay = self: super:
  with super;
  {
    llvmPackages_9 = callPackage ./llvm ({
      inherit (stdenvAdapters) overrideCC;
      buildLlvmTools = buildPackages.llvmPackages_9.tools;
      targetLlvmLibraries = targetPackages.llvmPackages_9.libraries;
    } // stdenv.lib.optionalAttrs (buildPackages.stdenv.cc.isGNU && stdenv.hostPlatform.isi686) {
        stdenv = overrideCC stdenv buildPackages.gcc6;
      }
    );
  }; in

let pkgs = (import ./nixpkgs-newer.nix) {
  overlays = [overlay];
 }; in

{
  pkgs = pkgs;
  clang_9 = pkgs.llvmPackages_9.clang-unwrapped;
  lld_9 = pkgs.llvmPackages_9.lld;
}
