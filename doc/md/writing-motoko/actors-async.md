---
sidebar_position: 2
---

# Actors



The programming model of the Internet Computer consists of memory-isolated canisters communicating by asynchronous message passing of binary data encoding Candid values. A canister processes its messages one-at-a-time, preventing race conditions. A canister uses call-backs to register what needs to be done with the result of any inter-canister messages it issues.

Motoko provides an **actor-based** programming model to developers to express **services**, including those of canister smart contracts on ICP. Each canister is represented as a typed actor. The type of an actor lists the messages it can handle. Each message is abstracted as a typed, asynchronous function. A translation from actor types to Candid types imposes structure on the raw binary data of the underlying Internet Computer. An actor is similar to an object, but is different in that its state is completely isolated, its interactions with the world are entirely through asynchronous messaging, and its messages are processed one-at-a-time, even when issued in parallel by concurrent actors.

# Actors

An actor is similar to an object, but is different in that:

- Its state is completely isolated.

- Its interactions with the world are done entirely through asynchronous messaging.

- Its messages are processed one-at-a-time, even when issued in parallel by concurrent actors.

All communication with and between actors involves passing messages asynchronously over the network using the Internet Computer’s messaging protocol. An actor’s messages are processed in sequence, so state modifications never admit race conditions, unless explicitly allowed by punctuating `await` expressions.

The Internet Computer ensures that each message that is sent receives a response. The response is either success with some value or an error. An error can be the explicit rejection of the message by the receiving canister, a trap due to an illegal instruction such as division by zero, or a system error due to distribution or resource constraints. For example, a system error might be the transient or permanent unavailability of the receiver (either because the receiving actor is oversubscribed or has been deleted).

In Motoko, actors have dedicated syntax and types:

- Messaging is handled by so called **shared** functions returning futures. Shared functions are accessible to remote callers and have additional restrictions: their arguments and return value must be shared types. Shared types are a subset of types that includes immutable data, actor references, and shared function references, but excludes references to local functions and mutable data.

- Future, `f`, is a value of the special type `async T` for some type `T`.

- Waiting on `f` to be completed is expressed using `await f` to obtain a value of type `T`. To avoid introducing shared state through messaging, for example, by sending an object or mutable array, the data that can be transmitted through shared functions is restricted to immutable, shared types.

- All state should be encapsulated within the actor or actor class. The main actor file should begin with imports, followed by the actor or actor class definition.

## Defining an actor

Consider the following actor declaration:

``` motoko file=../examples/counter-actor.mo
```

The `Counter` actor declares one field and three public, shared functions:

-   The field `count` is mutable, initialized to zero and implicitly `private`.

-   Function `inc()` asynchronously increments the counter and returns a future of type `async ()` for synchronization.

-   Function `read()` asynchronously reads the counter value and returns a future of type `async Nat` containing its value.

-   Function `bump()` asynchronously increments and reads the counter.

Shared functions, unlike local functions, are accessible to remote callers and have additional restrictions. Their arguments and return value must be shared type. Shared types are a subset of types that includes immutable data, actor references, and shared function references, but excludes references to local functions and mutable data. Because all interaction with actors is asynchronous, an actor’s functions must return futures, that is, types of the form `async T`, for some type `T`.

The only way to read or modify the state (`count`) of the `Counter` actor is through its shared functions.

A value of type `async T` is a future. The producer of the future completes the future when it returns a result, either a value or error.

Unlike objects and modules, actors can only expose functions, and these functions must be `shared`. For this reason, Motoko allows you to omit the `shared` modifier on public actor functions, allowing the more concise, but equivalent, actor declaration:

``` motoko name=counter file=../examples/counter-actor-sugar.mo
```

For now, the only place shared functions can be declared is in the body of an actor or actor class. Despite this restriction, shared functions are still first-class values in Motoko and can be passed as arguments or results, and stored in data structures.

The type of a shared function is specified using a shared function type. For example, the value `inc` has type `shared () → async Nat` and could be supplied as a standalone callback to some other service.

## Actor types

Just as objects have object types, actors have actor types. The `Counter` example above has the following type:

``` motoko no-repl
actor {
  inc  : shared () -> async ();
  read : shared () -> async Nat;
  bump : shared () -> async Nat;
}
```

Unlike objects and modules, actors can only expose functions, and these functions must be `shared`. For now, the only place shared functions can be declared is in the body of an actor or actor class. Despite this restriction, shared functions are still first-class values in Motoko and can be passed as arguments or results, and stored in data structures.

The `shared` modifier is required on every member of an actor. Motoko both elides them on display and allows you to omit them when authoring an actor type.

Thus, the previous type can be expressed more succinctly as:

``` motoko no-repl
actor {
  inc  : () -> async ();
  read : () -> async Nat;
  bump : () -> async Nat;
}
```

Like object types, actor types support subtyping: an actor type is a subtype of a more general one that offers fewer functions with more general types.


## Asynchronous behavior

Like other modern programming languages, Motoko permits an ergonomic syntax for **asynchronous** communication among components.

In the case of Motoko, each communicating component is an actor. As an example of using actors, consider this three-line program:

```motoko no-repl
let result1 = service1.computeAnswer(params);
let result2 = service2.computeAnswer(params);
finalStep(await result1, await result2)
```

This program’s behavior can be summarized as:

1.  The program makes two requests (lines 1 and 2) to two distinct services, each implemented as a Motoko actor or canister smart contract implemented in some other language.

2.  The program waits for each result to be ready (line 3) using the keyword `await` on each result value.

3.  The program uses both results in the final step (line 3) by calling the `finalStep` function.

The services **interleave** their executions rather than wait for one another, since this reduces overall latency. If you try to reduce latency this way without special language support, such interleaving will quickly sacrifice clarity and simplicity.

Even in cases where there are no interleaving executions, for example, if there were only one call above, not two, the programming abstractions still permit clarity and simplicity for the same reason. Namely, they signal to the compiler where to transform the program, freeing the programmer from contorting the program’s logic in order to interleave its execution with the underlying system’s message-passing loop.

In the above example, the program uses `await` in line 3 to express that interleaving behavior in a simple fashion.

In other programming languages that lack these abstractions, developers would not merely call these two functions directly, but would instead employ very advanced programming patterns, possibly registering developer-provided “callback functions” within system-provided “event handlers”. Each callback would handle an asynchronous event that arises when an answer is ready. This kind of systems-level programming is powerful, but very error-prone, since it decomposes a high-level data flow into low-level system events that communicate through shared state.

## Traps and commit points

A trap is a non-recoverable runtime failure caused by errors such as division-by-zero, out-of-bounds array indexing, numeric overflow, cycle exhaustion or assertion failure.

A shared function call that executes without executing an `await` expression never suspends and executes atomically. A shared function that contains no `await` expression is syntactically atomic.

### Commit points

An atomic shared function whose execution traps has no visible effect on the state of the enclosing actor or its environment - any state change is reverted, and any message that it has sent is revoked. In fact, all state changes and message sends are tentative during execution: they are committed only after a successful commit point is reached.

The points at which tentative state changes and message sends are irrevocably committed are:

-   Implicit exit from a shared function by producing a result.

-   Explicit exit via `return` or `throw` expressions.

-   Explicit `await` expressions.

### Traps

A trap will only revoke changes made since the last commit point. In particular, in a non-atomic function that does multiple awaits, a trap will only revoke changes attempted since the last await - all preceding effects will have been committed and cannot be undone.

Consider the following stateful `Atomicity` actor:

``` motoko no-repl file=../examples/atomicity.mo
```

Calling the shared function `atomic()` will fail with an error, since the last statement causes a trap. However, the trap leaves the mutable variable `s` with value `0`, not `1`, and variable `pinged` with value `false`, not `true`. This is because the trap happens before the method `atomic` has executed an `await`, or exited with a result. Even though `atomic` calls `ping()`, `ping()` is queued until the next commit point.

Calling the shared function `nonAtomic()` will also fail with an error due to a trap. In this function, the trap leaves the variable `s` with value `3`, not `0`, and variable `pinged` with value `true`, not `false`. This is because each `await` commits its preceding side-effects, including message sends. Even though `f` is complete by the second await, this await also forces a commit of the state, suspends execution and allows for interleaved processing of other messages to this actor.

<img src="https://github.com/user-attachments/assets/844ca364-4d71-42b3-aaec-4a6c3509ee2e" alt="Logo" width="150" height="150" />