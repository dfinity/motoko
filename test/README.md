The Motoko test suite
==========================

Commands
--------

Run these either in the top level directory, or in one of the subdirectories.

* `make`

   Runs all tests, fails if any fail.

* `make accept`

   Refreshes all output

* `make clean`

   Cleans

You can also run individual tests, for example:

    ./run.sh run/fac.mo

to run and

    ./run.sh -a run/fac.mo

to accept. Check `run.sh` for other flags (e.g. drun-mode), and see `Makefile`
in each subdirectory for the right flags for that directory.

Adding a new test
-----------------

1. Create `foo.mo` (or similar, see below)
2. Run `make accept` (or, more targeted, `../run.sh -a foo.mo`)
3. Add `foo.mo` and `ok/foo.*.ok` to git.

The `run.sh` script takes various flags, e.g. `-d` to compile actors instead of
program, `-p` for performance measurements. See the comment at the top of that
file. The each subdirectory has a `Makefile` specifying these flags.

Kinds of tests
--------------

The `run.sh` script supports different kinds of tests:

### Single Motoko file tests

These consist of a single Motoko file, e.g. `foo.mo`, which will be
typechecked, interpreted (in various variants) and run on `wasmtime` or (with
`-d`) `drun`/`ic-ref-run`.

With comments of the form `//SKIP run-low` individual phases can be skipped.
Similarly, mentioning the `uname` output (like `//SKIP Darwin`) skips the test
when running on that OS.

Comments of the form `//MOC-FLAG --package prim .` pass additional flags to
`moc`.

Comments of the form `//CALL` will be picked up and passed to `drun` or
`ic-ref-run` as additional calls to be made.  The variant `//OR-CALL` will
remove that line. This allows different behavior with the interpreter and in
`drun`. See existing files for details.

### Multiple Motoko test files

These only make sense with `-d`. Create a `foo.drun` file that is a mostly
unmodified input with one exception: You can reference `foo/bar.mo` files where
`drun` expects a `.wasm` file. `run.sh` will find these files, compile them to
`.wasm` and put that file name into the script before passing it to `drun`.

### Shell files

Files named `foo.sh` will simply be executed.

### `.wat` files

Files named `foo.wat` expect a corresponding `.c` file and are used to test
`mo-ld`. See `ld/` for examples.

### `.did` files

Files named `foo.did` will be passed through the `didc` file checker,
pretty-printer, the pretty-printed file will be checked again. It also generates
JS bindings, which will be parsed by `node`.


Running as a `nix` derivation
-----------------------------

You can run the test suite from the `motoko` (top-level) directory as:

``` shell
$ nix-build -A tests
```

You can also run individual directories via, say,

``` shell
$ nix-build -A tests.run-drun
```

Randomised testing
------------------

See `README.md` in the `random/` subdirectory.

Performance regression testing
------------------------------

The purpose of the `perf/` directory is to have a small (<20) set of test
programs representative of real use of Motoko.

For these tests the test suite records the following numbers:

* Size of the produced Wasm binary.
* Gas consumed by a single run in drun [not yet implemented]

The numbers are written to the file specified by `$PERF_OUT` (and end up being
the output of the nix derivation `tests.perf`.

The format is a simple CSV format, as consumed by
[gipeda](https://github.com/nomeata/gipeda).

Every PR reports a summary of changes to these numbers to the PR. [not yet implemented]
