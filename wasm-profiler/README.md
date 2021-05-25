A (hacky) Wasm instruction profiler
===================================

This repository contains a rather ad-hoc instruction counting profiler for Wasm.

The rust tool `wasm-profiler-instrument` (which you can run with `cargo run`)
will inject code into a Wasm module to count instructions, and print the
current counter value upon each function entry and exit, in an idiosyncratic format.

The printing can happen either via WASI’s `fd_write` on `stdout`
(`--wasi-system-api`) or via `ic0.debug_print` (`--ic-system-api`).

The values are printed in a way so they can be recognized by
`./wasm-profiler-postproc.pl` (even if mixed with other output), and turned
into [callgrind format] or [FlameGraph format].

[callgrind format]: https://valgrind.org/docs/manual/cl-format.html
[FlameGraph format]: https://github.com/brendangregg/FlameGraph

## Example

This example shows how to run this on a WASI compatible program

```
$ wget https://registry-cdn.wapm.io/contents/_/cowsay/0.2.0/target/wasm32-wasi/release/cowsay.wasm
$ wasmtime cowsay.wasm -- 'Hello World!'
 ______________
< Hello World! >
 --------------
        \   ^__^
         \  (oo)\_______
            (__)\       )\/\
               ||----w |
                ||     ||
$ cargo run -- -i cowsay.wasm -o cowsay-instrumented.wasm --wasi-system-api
$ wasmtime cowsay-instrumented.wasm -- 'Hello World!' > cowsay.out
$ head -n 3 cowsay.out
<pRfPAAAAAAAAAAAAAAAAAAAAAAA>
<pRfOCDAAAAALAAAAAAAAAAAAAAA>
<pRfHCDAAAAAFBAAAAAAAAAAAAAA>
$ ./wasm-profiler-postproc.pl flamegraph cowsay-instrumented.wasm < cowsay.out > cowsay.flamegraph
$ head -n 3 cowsay.flamegraph
_start 88
_start;__prepare_for_exit 2
_start;__prepare_for_exit;dummy 0
$ flamegraph.pl < cowsay.flamegraph > cowsay.svg
```

which produces

![A flamegraph for [cowsay](https://wapm.io/package/cowsay)](cowsay.svg)

## Contributions

are welcome, e.g. making this more robust, more efficient, or replacing the Perl script by Rust code.
