# Traps and commit points

## Overview

A trap is a non-recoverable runtime failure caused by errors such as division-by-zero, out-of-bounds array indexing, numeric overflow, cycle exhaustion or assertion failure.

A shared function call that executes without executing an `await` expression never suspends and executes atomically. A shared function that contains no `await` expression is syntactically atomic.

An atomic shared function whose execution traps has no visible effect on the state of the enclosing actor or its environment - any state change is reverted, and any message that it has sent is revoked. In fact, all state changes and message sends are tentative during execution: they are committed only after a successful commit point is reached.

The points at which tentative state changes and message sends are irrevocably committed are:

-   Implicit exit from a shared function by producing a result.

-   Explicit exit via `return` or `throw` expressions.

-   Explicit `await` expressions.

## Traps

A trap will only revoke changes made since the last commit point. In particular, in a non-atomic function that does multiple awaits, a trap will only revoke changes attempted since the last await - all preceding effects will have been committed and cannot be undone.

Consider the following stateful `Atomicity` actor:

``` motoko no-repl file=../examples/atomicity.mo
```

Calling the shared function `atomic()` will fail with an error, since the last statement causes a trap. However, the trap leaves the mutable variable `s` with value `0`, not `1`, and variable `pinged` with value `false`, not `true`. This is because the trap happens before the method `atomic` has executed an `await`, or exited with a result. Even though `atomic` calls `ping()`, `ping()` is queued until the next commit point.

Calling the shared function `nonAtomic()` will also fail with an error due to a trap. In this function, the trap leaves the variable `s` with value `3`, not `0`, and variable `pinged` with value `true`, not `false`. This is because each `await` commits its preceding side-effects, including message sends. Even though `f` is complete by the second await, this await also forces a commit of the state, suspends execution and allows for interleaved processing of other messages to this actor.