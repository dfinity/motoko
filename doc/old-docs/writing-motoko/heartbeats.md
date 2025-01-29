---
sidebar_position: 10
---

# Heartbeats



ICP canisters can elect to receive regular heartbeat messages by exposing a particular `canister_heartbeat` function (see [heartbeat](https://smartcontracts.org/docs/interface-spec/index.html#heartbeat)).

In Motoko, an actor can receive heartbeat messages by declaring a `system` function, named `heartbeat`, with no arguments returning a future of unit type (`async ()`).

## Using heartbeats

A simple, contrived example is a recurrent alarm, that sends a message to itself every `n`-th heartbeat:

``` motoko no-repl file=../examples/Alarm.mo
```

The `heartbeat` function is called on every ICP subnet heartbeat by scheduling an asynchronous call to the `heartbeat` function. Due to its `async` return type, a heartbeat function may send further messages and await results. The result of a heartbeat call, including any trap or thrown error, is ignored. The implicit context switch inherent to calling every Motoko async function means that the time the `heartbeat` body is executed may be later than the time the heartbeat was issued by the subnet.

As an `async` function, `Alarm`'s `heartbeat` function is free to call other asynchronous functions, as well as shared functions of other canisters.

<img src="https://github.com/user-attachments/assets/844ca364-4d71-42b3-aaec-4a6c3509ee2e" alt="Logo" width="150" height="150" />