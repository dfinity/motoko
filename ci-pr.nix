{ src ? { rev = null; }, base ? null }:
let
  nixpkgs = (import ./nix/nixpkgs.nix).nixpkgs { };

in
import ./ci.nix { inherit src; } // nixpkgs.lib.optionalAttrs (base != null) {
  perf-delta =
    let
      baseJobs = import "${base}/default.nix" { system = "x86_64-linux"; };
      prJobs = import ./default.nix { system = "x86_64-linux"; };
    in
    nixpkgs.runCommandNoCC "perf-delta" {
      baseStats = baseJobs.tests.perf;
      prStats = prJobs.tests.perf;
      nativeBuildInputs = [nixpkgs.coreutils nixpkgs.perl];
      diffStats = ./test/diff-stats.pl;
    } ''
      mkdir -p $out
      $diffStats $baseStats $prStats > $out/report;
      mkdir -p $out/nix-support
      echo "report perf-delta $out report" >> $out/nix-support/hydra-build-products
    '';
  }
