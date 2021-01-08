pkgs:
let
  wasm-profiler-src = pkgs.sources.wasm-profiler;
in {
  wasm-profiler-instrument =
    pkgs.rustPlatform.buildRustPackage {
      name = "wasm-profiler-instrument";

      src = wasm-profiler-src;

      # update this after dependency changes
      cargoSha256 = "0x7rg2w0vv9fx4mpbvgiwcmni2w71lrvi2i3019hrwlxn721pl47";
    };

  wasm-profiler-postproc = pkgs.stdenv.mkDerivation rec {
    name = "wasm-profiler-postproc";

    src = wasm-profiler-src;

    buildInputs = [ pkgs.perl ];

    installPhase = ''
      mkdir -p $out/bin
      cp $src/wasm-profiler-postproc.pl $out/bin/wasm-profiler-postproc
    '';
  };

  # the FlameGraph package is a bit inconvenient, with stuff like files.pl in
  # the path. Package a smaller, nicer one, with just the flamegraph tool.
  flamegraph-bin = pkgs.runCommandNoCC "flamegraph" {} ''
    mkdir -p $out/bin
    cp ${pkgs.flamegraph}/bin/flamegraph.pl $out/bin/flamegraph
  '';
}
