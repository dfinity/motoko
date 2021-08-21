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
  # This job compares the performance numbers of the current revision
  # with the mergeBase revision
  #
  # to test locally, use something like
  # nix-build -A perf-delta ci-pr.nix --arg src '{ mergeBase = builtins.fetchGit { url = ./.; rev = "2a6221425d2c482208f9f3e2e2fc00d4847212e5"; }; }'

nixpkgs.runCommandNoCC "perf-delta" {
  nativeBuildInputs = [ nixpkgs.coreutils diff-stats ];
} ''
  cat > $out
  if cmp -s ${wasm-hash-base} ${wasm-hash-pr}
  then
    echo "This PR does not affect the produced WebAssembly code." >> $out
  else
    diff-stats ${baseJobs.tests.perf}/stats.csv ${prJobs.tests.perf}/stats.csv >> $out;
  fi
''
