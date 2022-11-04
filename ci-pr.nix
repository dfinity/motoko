{ src ? { rev = null; }, labels ? {}, ... }:
let
  nixpkgs = import ./nix { };
  jobs = import ./ci.nix { inherit src; inherit labels; };
in
jobs // {
  all-jobs = nixpkgs.releaseTools.aggregate {
    name = "all-jobs";
    constituents = nixpkgs.lib.collect (drv: nixpkgs.lib.isDerivation drv) jobs;
  };
}
