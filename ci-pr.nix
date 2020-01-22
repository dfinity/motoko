{ src ? { rev = null; }, base ? null }:
let
  nixpkgs = import ./nix { };

  # Wrap in a derivation to fix path to perl in shebang
  diff-stats = nixpkgs.stdenvNoCC.mkDerivation {
    name = "diff-stats";
    src = ./test/diff-stats.pl;
    phases = [ "installPhase fixupPhase" ];
    buildInputs = [ nixpkgs.perl ];
    installPhase = ''
      mkdir -p $out/bin
      cp $src $out/bin/diff-stats
    '';
  };

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
      nativeBuildInputs = [ nixpkgs.coreutils diff-stats ];
    } ''
      mkdir -p $out
      diff-stats $baseStats $prStats > $out/report;
      mkdir -p $out/nix-support
      echo "report perf-delta $out report" >> $out/nix-support/hydra-build-products
      echo '{{{! comment:edit-one }}}' >> $out/comment
      cat $out/report >> $out/comment
      echo "comment manifest $out/comment" >> $out/nix-support/hydra-build-products
    '';
  }
