# Actors

## Overview

Motoko provides an **actor-based** programming model to developers to express **services**, including those of canister smart contracts on ICP.

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

## Defining an actor

Consider the following actor declaration:

```motoko
actor {
    public func greet(name : Text) : async Text {
        return "Hello, " # name # "!";
    };
};
```

Let’s take a look at a few key elements of this program:

- This sample code defines an `actor` instead of a main function, which some programming languages require. For Motoko, the main function is implicit in the file itself.

- Although the traditional "Hello, World!" program illustrates how you can print a string using a `print` or `println` function, that traditional program would not represent a typical use case for Motoko dapps that run on the Internet Computer.

- Instead of a `print` function, this sample program defines an `actor` with a public greet function that takes a `name` argument with a type of `Text`.

- The program uses the `async` keyword to indicate that it returns an asynchronous message consisting of a concatenated text string constructed using `"Hello, "`, the `#` operator, the `name` argument, and `"!"`.

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


## Asynchronous actors

Like other modern programming languages, Motoko permits an ergonomic syntax for **asynchronous** communication among components.

In the case of Motoko, each communicating component is an actor. As an example of using actors, consider this three-line program:

```motoko
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


