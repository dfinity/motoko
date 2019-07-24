The ActorScript RTS (C parts)
=============================

This directory contains the parts of the ActorScript runtime implemented in C.

tl;dr
-----

If you just want to get `as-rts.wasm` in this directory, run

    nix-shell --run 'make -C rts'

from the top-level directory of the ActorScript repository.

Compilation
-----------

Running `make` should produce `as-rts.wasm`.

If run within `nix-shell`, the environment variables `WASM_CLANG` and `WASM_LD`
should point to suitable binaries (we track a specific unreleased version of
`llvm`). If not present, the `Makefile` will try to use `clang-9` and
`wasm-ld-9`.

The runtime compiles and links in [libtommath]. It needs the source, so
`nix-build` and `nix-shell` will set the environment variable `TOMMATHSRC` to
point to the source in `/nix/store`.
If not present, the `Makefile` will look in `../../libtommath`, i.e. parallel to the 
`actorscript` repository; this is useful if you need to hack on libtommath.

[libtommath]: https://github.com/libtom/libtommath

Exporting and importing functions
---------------------------------

To export a function from C to ActorScript, use the `export` annotation. This
will make the function be visible in the final shared library.

To import a function, use the `from_rts` annotation, e.g. `alloc_bytes`.


libtommath and memory managment
-------------------------------

We have to make libtommath’s memory management (which expects C-like functions
`alloc`, `calloc` and `realloc`) work with the ActorScript runtime. See the
comment next to `mp_alloc` in `rts.c` for the techical details.

C tests
-------

To narrow down issues, or do regression testing on the C level, you can interact
with the code provided by `rts.c` from `test_rts.c`. With

    make test_rts && ./test_rts

this is executed. This is compiled natively, so may not hide bugs that are tied to
WebAssembly.

Static analysis with Frama-C
----------------------------

Some of the RTS code is pretty security-critical, as it deals with untrusted
data (in particular, the IDL parser). So we are experimenting with throwing
formal verification at it. The most promising tool in terms of power and
usabilty seems to be Frama-C, which should be installed if you if you
`nix-shell`; alternatively `opam install frama-c` works nicely.

We currently check that the IDL parser does not access any memory outside the
array given to it. The way to set this up in Frama-C is:

 * Write a “main” function that embodies a typical incarnation of the idl code.
   This is in `frama-main.c`.

 * Invoke the EVA (value analysis) plugin of Frama-C with suitable flags. See
   `run-frama.sh`.

   In particular, since we use pointer comparisons in ways that are not fully
   specified (or defiend, depending who you ask) we use
   `-eva-warn-undefined-pointer-comparison`. to allow that.

 * If there is a read from invalid memory, the script will error out
   (due to `-eva-stop-at-nth-alarm 0`).

   Else, a nice
   ```
   [inout] Inputs for function main:
       test[0..99]
   ```
   tells us that everything is right.

Frama-C doesn’t handle dynamic arrays very well, so we have to `split` the
execution for various array lengths in `frama-main.c`, as advised in
https://stackoverflow.com/a/57178643/946226.

Frama-C can also do more sophisticated verification, including complex
assertions, that we will need to explore.

So far this is an experiment, if it gets in the way, we can ditch it. Or if we
find a way to implement the RTS in Rust, we may also no longer need this.




