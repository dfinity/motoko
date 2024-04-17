# Timers

## Overview

ICP canisters can set an arbitrary number of single-expiration or recurring timers. See the [`Timer.mo`](base/Timer.md) module in the base library.

## Example

A simple example is a periodic reminder that logs a new year's message:

``` motoko no-repl file=../examples/Reminder.mo
```

The underlying mechanism is a [canister global timer](https://internetcomputer.org/docs/current/references/ic-interface-spec#timer) that by default is issued with appropriate callbacks from a priority queue maintained by the Motoko runtime.

The timer mechanism can be disabled completely by passing the `-no-timer` flag to `moc`.

## Low-level access

When lower-level access to the canister global timer is desired, an actor can elect to receive timer expiry messages by declaring a `system` function named `timer`. The function takes one argument used to reset the global timer and returns a future of unit type `async ()`.

If the `timer` system method is declared, the [`Timer.mo`](base/Timer.md) base library module may not function correctly and should not be used.

The following example of a global timer expiration callback gets called immediately after the canister starts, i.e. after install, and periodically every twenty seconds thereafter:

``` motoko no-repl
system func timer(setGlobalTimer : Nat64 -> ()) : async () {
  let next = Nat64.fromIntWrap(Time.now()) + 20_000_000_000;
  setGlobalTimer(next); // absolute time in nanoseconds
  print("Tick!");
}
```
