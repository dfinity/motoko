# This nix derivation calculates a comparison between the performance numbers of two commits.
# This is used on CI (see CI.md).
#
# It runs the old and the new moc on all tests files. If the output is
# identical, it reports that no changes happened.
#
# Else it compares the performance statistics from the `tests.perf` derivation.

{ ref, from, to }:
let
  nixpkgs = import ./nix { };

  subpath = p: import ./nix/gitSource.nix p;

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

  test-data = subpath ./test;

  wasm-hash-for = moc:
    nixpkgs.stdenvNoCC.mkDerivation {
      name = "wasm-hash";
      src = test-data;
      buildInputs = [ moc ];
      buildPhase = ''
        for file in */*.mo
        do
          # ignore all errors
          echo -n $file
          if timeout 10s moc $file -no-check-ir -ref-system-api -o $file.wasm 2>/dev/null
          then echo " failed (ignored)"
          else echo " ok"
          fi
        done

        if ! test -n "$(find . -name \*.wasm -print -quit)"
        then
          echo "No wasm files generated. wasm-hash-for broken?"
          exit 1
        fi
      '';
      installPhase = ''
        sha256sum **/*.wasm > $out
      '';
    };

  baseJobs = import (builtins.fetchGit {url = ./.; ref = ref; rev = from;}) {};
  prJobs = import (builtins.fetchGit {url = ./.; ref = ref; rev = to;}) {};

  # NB: We run both compilers on the new PR’s set of tests
  wasm-hash-base = wasm-hash-for baseJobs.moc;
  wasm-hash-pr = wasm-hash-for prJobs.moc;
in
nixpkgs.runCommandNoCC "perf-delta" {
  nativeBuildInputs = [ nixpkgs.coreutils diff-stats ];
} ''
  echo "Comparing from ${from} to ${to}:" > $out
  if cmp -s ${wasm-hash-base} ${wasm-hash-pr}
  then
    echo "The produced WebAssembly code seems to be completely unchanged." >> $out
  else
    diff-stats ${baseJobs.tests.perf}/stats.csv ${prJobs.tests.perf}/stats.csv >> $out;
  fi
''
