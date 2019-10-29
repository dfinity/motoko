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

to accept.

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

Randomised testing
------------------

See `README.md` in the `random/` subdirectory.
