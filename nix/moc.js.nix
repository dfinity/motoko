{ pkgs, commonBuildInputs, rts }:
let
  mk = n:
    pkgs.stdenv.mkDerivation {
      name = "${n}.js";
      src = ../src;
      buildInputs = commonBuildInputs pkgs ++ [
        pkgs.ocamlPackages.js_of_ocaml
        pkgs.ocamlPackages.js_of_ocaml-ppx
        pkgs.nodejs_20
        pkgs.nodePackages.terser
      ];
      buildPhase = ''
        patchShebangs .
      '' + pkgs.lib.optionalString (rts != null) ''
        ./rts/gen.sh ${rts}/rts/
      '' + ''
        make DUNE_OPTS="--profile=release" ${n}.js
        terser ${n}.js -o ${n}.min.js -c -m
      '';
      installPhase = ''
        mkdir -p $out
        mkdir -p $out/bin
        cp --verbose --dereference ${n}.js $out/bin
        cp --verbose --dereference ${n}.min.js $out/bin
      '';
      doInstallCheck = true;
      test = ../test + "/test-${n}.js";
      installCheckPhase = ''
        NODE_PATH=$out/bin node --experimental-wasm-memory64 $test
      '';
    };
in
{
  moc = mk "moc";
  moc_interpreter = mk "moc_interpreter";
  didc = mk "didc";
}
