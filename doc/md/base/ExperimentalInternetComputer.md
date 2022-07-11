# ExperimentalInternetComputer
Low-level interface to the Internet Computer.

**WARNING:** This low-level API is **experimental** and likely to change or even disappear.

## Value `call`
`let call : (canister : Principal, name : Text, data : Blob) -> async (reply : Blob)`

Calls ``canister``'s update or query function, `name`, with the binary contents of `data` as IC argument.
Returns the response to the call, an IC _reply_ or _reject_, as a Motoko future:

* The message data of an IC reply determines the binary contents of `reply`.
* The error code and textual message data of an IC reject determines the future's `Error` value.

Note: `call` is an asynchronous function and can only be applied in an asynchronous context.

## Function `countInstructions`
`func countInstructions(comp : () -> ()) : Nat64`

Given computation, `comp`, counts the number of actual and (for IC system calls) notional WebAssembly
instructions performed during the execution of `comp()`.

More precisely, returns the difference between the state of the IC instruction counter (_performance counter_ `0`) before and after executing `comp()`
(see [Performance Counter](https://internetcomputer.org/docs/current/references/ic-interface-spec#system-api-performance-counter)).

NB: `countInstructions(comp)` will _not_ account for any deferred garbage collection costs incurred by `comp()`.
