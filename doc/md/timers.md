# Timers

Internet Computer canisters can set an arbitrary number of single-expiration or recurring timers. See the `Timers.mo` module in the base library.

A simple, contrived example is a periodic reminder, that logs a new-year's message:

``` motoko file=./examples/Reminder.mo
```

The underlying mechanism is a [canister global timer](https://internetcomputer.org/docs/current/references/ic-interface-spec#timer) that, by default, is issued with appropriate callbacks from a priority queue maintained by the Motoko runtime. 

The timer mechanism can be disabled completely by passing the `-no-timer` flag to `moc`.

When lower-level access to the canister global timer is desired, an actor can elect to receive timer expiry messages by declaring a `system` function, named `timer`. The function takes one argument (to re-set the global timer), and returns a future of unit type (`async ()`).
If the `timer` system method is declared, the `Timers.mo` base library module may not function correctly and should not be used.

The following example of a global timer expiration callback gets called immmediately
after the canister starts (i.e. after install) and periodically every twenty second thereafter:
``` motoko
system func timer(setGlobalTimer : Nat64 -> ()) : async () {
  let next = Nat64.fromIntWrap(Time.now()) + 20_000_000_000;
  setGlobalTimer(next);
  print("Tick!");
}
```
