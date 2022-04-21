# Heartbeats {#_heartbeats}

{IC} canisters can elect to receive regular heartbeat messages by exposing a particular `canister_heartbeat` function (see [heartbeat](https://smartcontracts.org/docs/interface-spec/index.html#_heartbeat)).

In Motoko, an actor can receive heartbeat messages by declaring a `system` function, named `heartbeat`, with no arguments, returning a future of unit type (`async ()`).

A simple, contrived example is a recurrent alarm, that sends a message to itself every `n`-th heartbeat:

``` motoko
import Debug "mo:base/Debug";

actor Alarm {

  let n = 5;
  var count = 0;

  public shared func ring() : async () {
    Debug.print("Ring!");
  };

  system func heartbeat() : async () {
    if (count % n == 0) {
      await ring();
    };
    count += 1;
  }
}
```

The `heartbeat` function, when declared, is called on every {IC} subnet *heartbeat*, by scheduling an asynchronous call to the `heartbeat` function. Due to its `async` return type, a heartbeat function may send further messages and await results. The result of a heartbeat call, including any trap or thrown error, is ignored. The implicit context switch inherent to calling every Motoko async function, means that the time the `heartbeat` body is executed may be later than the time the heartbeat was issued by the subnet.

As an `async` function, `Alarm`'s `hearbeat` function is free to call other asynchronous functions (the inner call to `ring()` above is an example), as well as shared functions of other canisters.
