# QA tool `qc-actorscript`

This tool generates ActorScript AST fragments and executes them,
checking for the same outcome as the Haskell (here: reference)
implementation.

In order to compile and test it expects following tools to be in the
`PATH`:
- `asc` (the ActorScript compiler)
- `wasm-interp` (part of `nixpkgs.wabt`)

## How to spot failures

`qc-actorscript` is run from the CI infrastructure on each commit. On failures, something like

```
  Properties
    (checked by QuickCheck)
      expected failures:  FAIL (0.08s)
        *** Failed! Assertion failed (after 1 test):
        Failing "let _ = (-(nat32ToNat(((((0) : Nat32) * ((65535) : Nat32)) % (((65535) : Nat32) * ((0) : Nat32))))));"
        Use --quickcheck-replay=232458 to reproduce.
      expected successes: OK (13.22s)
        +++ OK, passed 100 tests.

1 out of 2 tests failed (13.30s)
```

will appear in the CI log. The relevant recipe for reliable reproduction is the
`--quickcheck-replay=232458` hint.

## How to run locally

With this piece of information you can run the identical test locally,
using the following `nix-build` invocation from your `actorscript`
directory:
``` shell
$ nix-build -A tests --arg replay 232458
```

## Running in `asc` and `wasm-interp`

Of course you can dump the failing test case into a file, compile it
to WASM, and execute it in (e.g.) `wasm-interp`:

``` shell
$ asc -no-dfinity-api snippet.as
$ wasm-interp --enable-multi snippet.wasm
```

In tests under category *expected failures*, this should trap.
For *expected successes* it should execute without errors.

## 0 -- 2019-08-08

* First version. Released on a suspecting world. See also [GitHub issue 609](https://github.com/dfinity-lab/actorscript/pull/609).
