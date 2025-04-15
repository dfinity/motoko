# This nix derivation calculates a comparison between the performance numbers of two commits.
# This is used on CI (see CI.md).
#
# It runs the old and the new moc on all tests files. If the output is
# identical, it reports that no changes happened.
#
# Else it compares the performance statistics from the `tests.perf` derivation.

{ ref, from, to }:
let
  flake = builtins.getFlake (toString ./.);
  pkgs = import flake.inputs.nixpkgs {};
  system = builtins.currentSystem;

  # Wrap in a derivation to fix path to perl in shebang
  diff-stats = pkgs.stdenvNoCC.mkDerivation {
    name = "diff-stats";
    src = ./test/diff-stats.pl;
    phases = [ "installPhase fixupPhase" ];
    buildInputs = [ pkgs.perl ];
    installPhase = ''
      mkdir -p $out/bin
      cp $src $out/bin/diff-stats
    '';
  };

  wasm-hash-for = moc:
    pkgs.stdenvNoCC.mkDerivation {
      name = "wasm-hash";
      src = ./test;
      buildInputs = [ moc ];
      buildPhase = ''
        moc --version
        for file in */*.mo
        do
          # ignore all errors
          echo -n $file
          if timeout 10s moc $file --omit-metadata motoko:compiler -no-check-ir -ref-system-api -o $file.wasm 2>/dev/null
          then echo " ok"
          else echo " failed (ignored)"
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

  flakeOf = rev:
    let
      checkout = builtins.fetchGit {url = ./.; ref = ref; inherit rev;};
      flakePath = builtins.unsafeDiscardStringContext "${checkout}";
    in builtins.getFlake flakePath;

  baseFlake = flakeOf from;
  prFlake = flakeOf to;

  # NB: We run both compilers on the new PR’s set of tests
  wasm-hash-base = wasm-hash-for baseFlake.packages.${system}.debug.moc;
  wasm-hash-pr = wasm-hash-for prFlake.packages.${system}.debug.moc;
in
pkgs.runCommandNoCC "perf-delta" {
  nativeBuildInputs = [ pkgs.coreutils diff-stats ];
} ''
  echo "Comparing from ${from} to ${to}:" > $out
  if cmp -s ${wasm-hash-base} ${wasm-hash-pr}
  then
    echo "The produced WebAssembly code seems to be completely unchanged." >> $out
  else
    diff-stats \
      ${baseFlake.checks.${system}.perf}/stats.csv \
      ${prFlake.checks.${system}.perf}/stats.csv >> $out;
  fi
''
