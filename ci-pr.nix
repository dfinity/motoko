{ src ? { rev = null; }, base ? null }:
let
  nixpkgs = (import ./nix/nixpkgs.nix).nixpkgs { };
  prJobs = import ./ci.nix { inherit src; };
in
prJobs // nixpkgs.lib.optionalAttrs (base != null) (
  let
    baseJobs = import "${base}/ci.nix" { src = base; };
  in {
    diff-from-base = {
      produce-exchange = nixpkgs.runCommandNoCC "produce-exchange-diff-from-base" {
        base = baseJobs.produce-exchange;
        pr = prJobs.produce-exchange;
        nativeBuildInputs = [nixpkgs.coreutils nixpkgs.gawk];
      } ''
        size() {
          du $1 | awk '{print $1}'
        }

        sizeBase=$(size "$base/ProduceExchange.wasm")
        sizePr=$(size "$pr/ProduceExchange.wasm")
        diff=$(($sizePr - $sizeBase))

        mkdir -p $out/nix-support

        (cat <<EOI
        base = "$base"
        pr   = "$pr"
        size "\$base/ProduceExchange.wasm" = $sizeBase
        size "\$pr/ProduceExchange.wasm"   = $sizePr
        diff = $diff
        EOI
        ) > $out/produce-exchange-diff-from-base

        echo "report produce-exchange-diff-from-base $out produce-exchange-diff-from-base" >> \
          $out/nix-support/hydra-build-products
      '';
    };
  }
)
