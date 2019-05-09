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

If run within `nix-shell`, the environment variables `CLANG` and `WASM_LD`
should point to suitable binaries. If not present, the `Makefile` will try to
use `clang-9` and `wasm-ld-9`.

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
