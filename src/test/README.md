The ActorScript test suite
==========================

Commands
--------

* `make` (or `make stats`)

   Runs all tests, fails if any fail.

* `make test.run`

   Runs `asc -r` on `test.as`, and writes the output to `test.run`

   Analogous: `make test.wat`, `make test.wat.stderr` `make test.wat-run`

* `make test.run.diff`

   Writes the difference between `test.run` and `test.run.ok` to `test.run.diff`

* `make test.run.refresh`

   Copies `test.run` to `test.run.ok`.

* `make current`  (or `make accept)

   Refreshes all output

Adding a new test
-----------------

1. Create `foo.as`
2. Run `make accept` (or, more targeted, `make foo.refresh`)
3. Add `foo.as` and `foo.ok` to git.
