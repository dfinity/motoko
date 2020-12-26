The Motoko RTS (C and Rust parts)
=================================

This directory contains the parts of the Motoko runtime implemented in C and
Rust.

tl;dr
-----

If you just want to get `mo-rts.wasm` in this directory, run

    nix-shell --run 'make -C rts'

from the top-level directory of the Motoko repository.

Compilation
-----------

Running `make` should produce `mo-rts.wasm`.

If run within `nix-shell`, the environment variables `WASM_CLANG` and `WASM_LD`
should point to suitable binaries (we track a specific unreleased version of
`llvm`). If not present, the `Makefile` will try to use `clang-10` and
`wasm-ld-10`.

The runtime compiles and links in [libtommath]. It needs the source, so
`nix-build` and `nix-shell` will set the environment variable `TOMMATHSRC` to
point to the source in `/nix/store`.
If not present, the `Makefile` will look in `../../libtommath`, i.e. parallel to the 
`motoko` repository; this is useful if you need to hack on libtommath.

[libtommath]: https://github.com/libtom/libtommath

Exporting and importing functions
---------------------------------

To export a function from C to Motoko, use the `export` annotation. This
will make the function be visible in the final shared library.

To import a function, use the `from_rts` annotation, e.g. `alloc_bytes`.

libtommath and memory managment
-------------------------------

We have to make libtommath’s memory management (which expects C-like functions
`alloc`, `calloc` and `realloc`) work with the Motoko runtime. See the
comment next to `mp_alloc` in `rts.c` for the techical details.

Generating libtommath bindings
------------------------------

Use [bindgen](https://github.com/rust-lang/rust-bindgen) with these parameters:

```
bindgen <path to tommath.h> \
    -o tommath_bindings.rs \
    --use-core --ctypes-prefix=libc --no-layout-tests \
    --whitelist-function mp_init \
    --whitelist-function mp_set_u32 \
    --whitelist-function mp_set_i32 \
    --whitelist-function mp_get_i32 \
    --whitelist-function mp_set_u64 \
    --whitelist-function mp_set_i64 \
    --whitelist-function mp_get_i64 \
    --whitelist-function mp_isneg \
    --whitelist-function mp_count_bits \
    --whitelist-function mp_get_mag_u32 \
    --whitelist-function mp_get_mag_u64 \
    --whitelist-function mp_cmp \
    --whitelist-function mp_add \
    --whitelist-function mp_sub \
    --whitelist-function mp_mul \
    --whitelist-function mp_div \
    --whitelist-function mp_neg \
    --whitelist-function mp_abs \
    --whitelist-function mp_mul_2d \
    --whitelist-function mp_expt_u32 \
    --whitelist-function mp_2expt \
    --blacklist-type __int32_t \
    --blacklist-type __int64_t \
    --blacklist-type __uint32_t \
    --blacklist-type __uint64_t
```

Whitelist parameters used as libtommath.h has lots of definitions that we don't
need. Blacklist parameters are used because bindgen still generates unused type
definition with the whitelist parameters.

Note that bindgen can't generate Rust macros or functions for CPP macros, so
macros like `mp_get_u32` and `mp_isneg` need to be manually implemented.

C tests
-------

To narrow down issues, or do regression testing on the C level, you can interact
with the code provided by `rts.c` from `test_rts.c`. With

    make test_rts && ./test_rts

this is executed. This is compiled natively, so may not uncover bugs that are tied to
WebAssembly.

Rust build
----------

The rust parts are built from `motoko-rts`, using `xargo` and `cargo`.

To build this in nix, we need pre-fetch some dependencies (currently
`compiler_builtins` and `libc`). This works in `nix-build` by:

 * Adding`compiler_builtins` as a dependency in `Cargo.toml` (even though not
   needed), so that it shows up with a hash in `Cargo.lock`

 * Building a directory with vendored sources in `default.nix` (see `rustDeps`)

 * Configuring `cargo` to use that vendored directory (see `preBuild`)

If you change dependencies (e.g. bump versions, add more crates),

 1. Add them to `Cargo.toml`
 2. Make sure that `Cargo.lock` is up to date
 3. In `default.nix`, invalidate the `sha256` of `rustDeps` (e.g. change one
    character)
 4. Run `nix-build -A rts`. You should get an error message about the actual
    checksum.
 5. Set that as `sha256` of `rustDeps` in `default.nix`

Warning: nix will happily use a stale verion of the dependencies if you do not
do step 3.

AFL tests
---------

Some tests are best run by AFL (American Fuzzy Lop), which is a whitebox
fuzzer that instruments the code to get more coverage.

You can install `afl` using `nix-env -i afl`.

You need to compile the code with instrumentation and then run it using the
helper script provided:

```
make clean
CLANG=afl-gcc make test_leb128
./run-afl-leb128.sh
```


