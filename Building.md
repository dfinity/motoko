## Installation using Nix

If you want just to _use_ `moc`, you can install the `moc` binary into your nix
environment with
```
$ nix-env -i -f . -A moc
```


## Development using Nix

To enter a shell with the necessary dependencies available, use
```
$ nix-shell
```

Within this shell you can run
 * `make` in `src/` to build all binaries,
 * `make moc` in `src/` to build just the `moc` binary,
 * `make DUNE_OPTS=--watch moc` to keep rebuilding as source files are changing
 * `make` in `rts/` to build the Motoko runtime
 * `make` in `test/` to run the test suite.

This invokes `dune` under the hood, which will, as a side effect, also create
`.merlin` files for integration with Merlin, the Ocaml Language Server


## Replicating CI locally

A good way to check that everything is fine, i.e. if this will pass CI, is to run
```
$ nix-build --no-out-link
```


## Development without nix-shell

You can get a development environment without having to use `nix-shell`
(although installing all required tools without nix is out of scope).

 * Use your system’s package manager to install `ocaml` (4.07) and
   [`opam`](https://opam.ocaml.org/doc/Install.html)
 * Install the packages:
   ```
   opam install num vlq yojson menhir stdio js_of_ocaml js_of_ocaml-ppx ppx_inline_test atdgen wasm obelisk
   ```
 * Install into your `PATH` various command line tools used by, in particular,
   the test suite:
   ```
   nix-env -i -f . -A wasmtime
   nix-env -i -f . -A filecheck
   nix-env -i -f . -A wabt
   nix-env -i -f . -A drun
   nix-env -i -f . -A ic-run
   ```
 * Building the Motoko runtime without nix is tricky. But you can run
   ```
   nix-shell --run 'make -C rts'
   ```
   to get `rts/mo-rts.wasm`.
 * Add `./bin` to your `$PATH` so that the testsuite will find the build
   products (see `./bin/wrapper.sh` for details).

## Profile the compiler

1. Build with profiling within nix-shell (TODO: How to do with dune)
   ```
   make -C src clean
   make BUILD=p.native -C src moc
   ```
2. Run `moc` as normal, e.g.
   ```
   ./src/moc -g -c foo.mo -o foo.wasm
   ```
   this should dump a `gmon.out` file in the current directory.
3. Create the report, e.g. using
   ```
   gprof --graph src/moc
   ```
   (Note that you have to _run_ this in the directory with `gmon.out`, but
   _pass_ it the path to the binary.)


## Updating Haskell Packages

When the `.cabal` file of a Haskell package is changed you need to make sure the
corresponding `default.nix` file (stored in `nix/generated/`) is kept in sync
with it.

As mentioned in the `nix/generate.nix` files, these files are automatically
generated. See `nix/generate.nix` for the command to update them.

Don't worry if you forget to update the `default.nix` file, the CI job
`check-generated` checks if these files are in sync and fail with a diff if
they aren't.
