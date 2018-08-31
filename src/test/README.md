The ActorScript test suite
==========================

Commands
--------

* `make` (or `make stats`)

   Runs all tests, fails if any fail.

* `make test.out`

   Runs `asc` on `test.as`, and writes the output to `test.out`

* `make test.diff`

   Writes the difference between `test.out` and `test.ok` to `test.diff`

* `make test.refresh`

   Copies `test.out` to `test.ok`.

* `make current`  (or `make accept)

   Refreshes all output

Adding a new test
-----------------

1. Create `foo.as`
2. Run `make accept` (or, more targeted, `make foo.refresh`)
3. Add `foo.as` and `foo.ok` to git.
