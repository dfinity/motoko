The ActorScript test suite
==========================

Commands
--------

* `make` (or `make stats`)

   Runs all tests, fails if any fail.

* `make _out/test.run`

   Runs `asc -r` on `test.as`, and writes the output to `_out/test.run`

   Analogous:

   - `make _out/test.tc`
   - `make _out/test.wat`
   - `make _out/test.wat.stderr`
   - `make _out/test.wat-run`

* `make test.run.diff`

   Writes the difference between `_out/test.run` and `ok/test.run.ok` to
   `_out/test.run.diff`

* `make test.run.refresh`

   Copies `_out/test.run` to `ok/test.run.ok`.

* `make test.refresh`

   Refreshes all output for `test`

* `make current` (or `make accept)

   Refreshes all output

Adding a new test
-----------------

1. Create `foo.as`
2. Run `make accept` (or, more targeted, `make foo.refresh`)
3. Add `foo.as` and `ok/foo.*.ok` to git.
