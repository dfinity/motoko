# Timers

Internet Computer canisters can set an arbitrary number of single-expiration or recurring timers. See the `Timers.mo` module in the base library.

The underlying mechanism is a [canister global timer](https://internetcomputer.org/docs/current/references/ic-interface-spec#timer) that is reprogrammed according to an internally kept priority queue.

The timer mechanism can be disabled completely by passing the `-no-timer` flag to `moc`.

When utmost control about the global timer is necessary, an actor can receive global timer expiry messages by declaring a `system` function, named `timer`, with one argument (to set the global timer), returning a future of unit type (`async ()`). In this case the `Timers.mo` base library module is not functional and shouldn't be used.
