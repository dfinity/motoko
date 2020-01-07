The Motoko test suite
==========================

Commands
--------

Run these either in the top level directory, or in one of the subdirectories.

* `make`

   Runs all tests, fails if any fail.

* `make accept`

   Refreshes all output

* `make coverage`

   Creates a coverage report in `./coverage/`

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

1. Create `foo.mo`
2. Run `make accept` (or, more targeted, `../run.sh -a foo.mo`)
3. Add `foo.mo` and `ok/foo.*.ok` to git.


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

The numbers are written to `_out/stats.csv`, and are also the output of the nix
derivation `tests.perf`.

The format is a simple CSV format, as consumed by
[gipeda](https://github.com/nomeata/gipeda).

Every PR reports a summary of changes to these numbers to the PR. [not yet implemented]
