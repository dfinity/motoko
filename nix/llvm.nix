{ system } :

let pkgs = (import ./nixpkgs-llvm.nix) {
  system = system;
 }; in

{
  pkgs = pkgs;
  clang_9 = pkgs.llvmPackages_9.clang-unwrapped;
  lld_9 = pkgs.llvmPackages_9.lld;
}
