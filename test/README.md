The ActorScript test suite
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

    ./run.sh run/fac.as

to run and

    ./run.sh -a run/fac.as

to accept.

Adding a new test
-----------------

1. Create `foo.as`
2. Run `make accept` (or, more targeted, `../run.sh -a foo.as`)
3. Add `foo.as` and `ok/foo.*.ok` to git.
