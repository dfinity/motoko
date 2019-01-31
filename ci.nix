# dev is now passed in from hydra
{ dev }:
let dvm = (import "${dev}/default.nix" {}).dvm;
in import ./default.nix { inherit dvm; }
