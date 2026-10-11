# Compute plugins

Build from the repository root inside `nix develop`:

```sh
make -C rts
plugins/build.sh
```

This produces two independent packages in `_build/compute-plugins/`, each
containing `plugin.cmxs` and `runtime.wasm`, and builds the matching native `moc`.

```sh
moc --plugin _build/compute-plugins/candidx \
    --plugin _build/compute-plugins/sha256 app.mo -o app.wasm
```

When using prebuilt packages and a compatible native compiler, only the `moc`
command is needed. Each `--plugin` dynamically loads a compiler extension for
that invocation; it does not rebuild `moc`. Referenced Wasm kernels are linked
into the canister, preserving direct calls. The `.cmxs` packages must match the
compiler/OCaml build and host platform. See
[loading prebuilt plugins](../design/ComputePlugins.md#loading-prebuilt-plugins-during-canister-compilation).

```motoko
import Prim "mo:⛔";
let digest = Prim.sha256(Prim.to_candid_canonical({ value = 42 }));
```

Use the same flags with `moc -r`, `moc -r -iR`, or `moc --check`.
See [the design and ABI](../design/ComputePlugins.md) for specialization,
temporary allocation, GC invariants, limitations, and authoring another plugin.
The compiler components live in [`src/compute_plugins/`](../src/compute_plugins/);
the Wasm kernels live in [`runtime/`](runtime/).

Run integration tests after the build:

```sh
test-runner -b test/compute-plugins/run
test-runner -b test/compute-plugins/fail
test-runner -b test/compute-plugins/trap
test-runner -b test/compute-plugins/run-drun
python3 test/compute-plugins/packaging.py
```

Run the kernels' native unit tests independently:

```sh
cargo test --manifest-path plugins/runtime/Cargo.toml --locked
```

The [compute plugin CI workflow](../.github/workflows/compute-plugins.yml) builds
matching packages and runs these suites, packaging checks, native kernel tests,
and linker fixture validation and execution.

For IC instruction and allocation measurements, use the
[PocketIC benchmark harness](../test/compute-plugins/bench/README.md). It checks
output equivalence and compares the same workloads across compiler/plugin
builds, including empty inputs, large Blobs, sparse records, and variant arrays.
The [second optimization pass](../test/compute-plugins/bench/results/pass2/README.md)
also covers arrays of fixed-shape records, repeated variant tags, and SHA's
32-byte and 64-byte specializations, including their memory and code-size costs.
