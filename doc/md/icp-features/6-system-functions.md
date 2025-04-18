---
sidebar_position: 6
---

# System functions

ICP supports five system functions that canisters can call to interact with the ICP runtime environment:

- [`heartbeat`](https://internetcomputer.org/docs/references/ic-interface-spec#heartbeat)
- [`timer`](https://internetcomputer.org/docs/references/ic-interface-spec#global-timer)
- [`preupgrade`](#preupgrade)
- [`postupgrade`](#postupgrade)
- [`inspect`](https://internetcomputer.org/docs/references/ic-interface-spec#system-api-inspect-message)

Declaring any other system function will result in an error. Canisters can use these functions to efficiently manage state transitions, automate tasks, or handle system-level operations. 

## `heartbeat()`

Canisters can opt to receive [heartbeat messages](https://internetcomputer.org/docs/building-apps/network-features/periodic-tasks-timers#heartbeats) by exposing a `canister_heartbeat` function. In Motoko, this is achieved by declaring the system function `heartbeat`, which takes no arguments and returns an asynchronous unit type (`async ()`).

Since `heartbeat()` is async, it can invoke other asynchronous functions and await their results. This function executes on every **subnet heartbeat**, enabling periodic task execution without requiring external triggers. Since subnet heartbeats operate at the protocol level, their timing is not precise and depends on network conditions and execution load. As a result, using heartbeats for **high-frequency or time-sensitive operations should be done cautiously**, as there is no guarantee of real-time execution.

Every async call in Motoko introduces a context switch, therefore the actual execution of the heartbeat function may be delayed relative to when the subnet issues the heartbeat. The function's result is ignored, so any errors or traps do not affect subsequent executions.

If a canister exports a function named `canister_heartbeat`, it must have the type `() -> ()`, ensuring it adheres to the expected [system function signature](https://internetcomputer.org/docs/references/ic-interface-spec#heartbeat).

One example use case for [heartbeats](https://internetcomputer.org/docs/building-apps/network-features/periodic-tasks-timers#heartbeats) is a recurring alarm, where the canister performs an action at regular intervals. In the example below, the canister triggers an event every fifth heartbeat by maintaining a counter.

```motoko no-repl
import Debug "mo:base/Debug";

persistent actor Alarm {

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

## `timer()`

The [`timer()` system function](https://internetcomputer.org/docs/building-apps/network-features/periodic-tasks-timers#timers) allows canisters to schedule a single execution of a task after a specified delay. To make it repeat, the function must explicitly reset the timer using `setGlobalTimer()` within its body. It takes a single argument for resetting the global timer and returns `async ()`. 

Unlike `heartbeat()`, which runs automatically on every subnet heartbeat, `timer()` requires **manual rescheduling** after execution. This gives canisters precise control over whether the [timer](https://internetcomputer.org/docs/building-apps/network-features/periodic-tasks-timers#timers) runs once or continuously, depending on when (or if) `setGlobalTimer()` is called again.

In the following example, `timer()` runs once immediately after deployment, then stops.

```motoko no-repl
import Debug "mo:base/Debug";

system func timer(_ : Nat64 -> ()) : async () {
  Debug.print("Timer triggered!");
  // No call to setGlobalTimer() → the timer does not repeat.
}
```

To run the timer every 20 seconds, it must be explicitly rescheduled.

```motoko no-repl
import Time "mo:base/Time";
import Debug "mo:base/Debug";

system func timer(setGlobalTimer : Nat64 -> ()) : async () {
  let next = Nat64.fromIntWrap(Time.now()) + 20_000_000_000; // 20 seconds
  setGlobalTimer(next); // Reschedule for next execution
  Debug.print("Repeating Timer Triggered!");
}
```

## `preupgrade()`

The `preupgrade()` system function is called immediately before a canister upgrade to prepare for state migration by saving necessary data. It is primarily used to transfer non-stable variables into stable storage, ensuring that important state information persists across upgrades, as `preupgrade()` runs before the new Wasm module is installed.

Any failure, such as a trap or exceeding computation limits prevents the upgrade from succeeding, potentially leaving the canister in an unrecoverable state. As a result, **using `preupgrade()` is discouraged unless necessary**.

The following example saves a `HashMap` of user balances into a stable variable before upgrading.

`balances` is a non-stable `HashMap` that would normally be lost during an upgrade. Before upgrading, `preupgrade()` stores the key-value pairs in a stable array (`savedBalances`).

After upgrading, the `postupgrade()` function (#postupgrade) can restore the saved state.

```motoko no-repl
import Iter mo:base/Iter";
import HashMap "mo:base/HashMap";

persistent actor Token {

  transient var balances = HashMap.HashMap<Text, Nat>(10, Text.equal, Text.hash); // Non-stable
  var savedBalances : [(Text, Nat)] = []; // implicit stable storage

  system func preupgrade() {
    savedBalances := Iter.toArray(balances.entries()); // Save state before upgrade
  }
}
```


## `postupgrade()`

The `postupgrade()` system function is called immediately after a canister upgrade, allowing a canister to restore state or execute initialization logic. Unlike `preupgrade()`, **it is not required**, as most of its effects can be achieved using actor initialization expressions (`let` bindings and expression statements). However, `postupgrade()` is useful for reconstructing data structures or running migration logic.

This example restores the `balances` `HashMap` using the data saved in `preupgrade()`.

```motoko no-repl
import HashMap "mo:base/HashMap";

persistent actor Token {

  transient var balances = HashMap.HashMap<Text, Nat>(10, Text.equal, Text.hash);
  var savedBalances : [(Text, Nat)] = [];

  system func postupgrade() {
    balances := HashMap.fromIter(savedBalances.vals(), 10, Text.equal, Text.hash);
  }
}
```

The **use of upgrade hooks is not recommended** as they can fail and cause the program to enter an unrecoverable state. With advancements in orthogonal persistence, these hooks are expected to be deprecated.

In many cases, stable variables or actor initialization expressions can replace `postupgrade()`. Complex transformations increase the risk of errors and failures.

## `inspect()`

The [`inspect()` system function](https://internetcomputer.org/docs/references/ic-interface-spec#system-api-inspect-message) allows a canister to inspect ingress messages before execution, determining whether to accept or reject them. The function receives a record of message attributes, including the caller’s principal, the raw argument `Blob`, and a variant identifying the target function.
 
It returns a `Bool`, where `true` permits execution and `false` rejects the message. Similar to a [query](https://internetcomputer.org/docs/building-apps/essentials/message-execution), any side effects are discarded. If `inspect()` traps, it is equivalent to returning `false`. Unlike other system functions, the argument type of `inspect()` depends on the actor's exposed interface, meaning it can selectively handle different methods or ignore unnecessary fields. 

However, `inspect()` should not be used for definitive access control because it runs on a single replica without going through consensus, making it susceptible to boundary node spoofing. Additionally, `inspect()` only applies to [ingress messages](https://internetcomputer.org/docs/building-apps/essentials/message-execution), not [inter-canister calls](https://internetcomputer.org/docs/references/async-code), meaning secure access control must still be enforced within shared functions.

The following actor defines an inspect function that blocks anonymous callers, limits message size, and rejects specific argument values.

```motoko no-repl
import Principal "mo:base/Principal";

persistent actor Counter {
  
  var c = 0;

  public func inc() : async () { c += 1 };
  public func set(n : Nat) : async () { c := n };
  public query func read() : async Nat { c };
  public func reset() : () { c := 0 }; // One way function

  system func inspect(
    {
      caller : Principal;
      arg : Blob;
      msg : {
        #inc : () -> ();
        #set : () -> Nat;
        #read : () -> ();
        #reset : () -> ();
      }
    }) : Bool {

    if (Principal.isAnonymous(caller)) return false; // Reject anonymous calls
    if (arg.size() > 512) return false; // Reject messages larger than 512 bytes

    switch (msg) {
      case (#inc _) { true };   // Allow increment
      case (#set n) { n() != 13 }; // Reject setting counter to 13
      case (#read _) { true };  // Allow reading the counter
      case (#reset _) { false }; // Reject reset calls
    }
  }
}
```
