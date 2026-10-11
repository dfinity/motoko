# Compiler plugin infrastructure tests

These small fixtures test the compiler's plugin API independently of production
plugins. `echo` copies a Blob through shared Wasm64 memory; `typed` specializes a
variadic intrinsic using its argument types and exercises managed scratch
allocation. Their runtimes intentionally export the same function name with
different signatures, so composition also checks symbol isolation.

From the Motoko repository root, in its Nix development shell:

```sh
test/compute-plugins/build.sh
test-runner -b test/compute-plugins/run
test-runner -b test/compute-plugins/fail
test-runner -b test/compute-plugins/trap
test-runner -b test/compute-plugins/run-drun
python3 test/compute-plugins/packaging.py
```

The build uses a dynamically linked native `moc` and matching compiler
interfaces. Pass `--skip-motoko-build` when that compiler is already built.
Fixtures are emitted to `_build/compute-plugin-tests`; no Rust build or external
plugin repository is required. Source/IR interpreters and Wasm execution must
agree, diagnostics use normal expectations, and the IC test retains plugin
results across forced GC. `packaging.py` checks loader compatibility and normal
diagnostics for invalid packages. Linker-specific validation lives in
`src/linking/test_linkModule.ml`.

Production CandidX/SHA-256 implementations, conformance tests, and benchmarks
live in [motoko_plugins](https://github.com/Neutrinomic/motoko_plugins).
