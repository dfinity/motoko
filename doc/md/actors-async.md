# Actors and async data

The programming model of the Internet Computer consists of memory-isolated canisters communicating by asynchronous message passing of binary data encoding Candid values. A canister processes its messages one-at-a-time, preventing race conditions. A canister uses call-backs to register what needs to be done with the result of any inter-canister messages it issues.

Motoko abstracts the complexity of the Internet Computer with a well known, higher-level abstraction: the *actor model*. Each canister is represented as a typed actor. The type of an actor lists the messages it can handle. Each message is abstracted as a typed, asynchronous function. A translation from actor types to Candid types imposes structure on the raw binary data of the underlying Internet Computer. An actor is similar to an object, but is different in that its state is completely isolated, its interactions with the world are entirely through asynchronous messaging, and its messages are processed one-at-a-time, even when issued in parallel by concurrent actors.

In Motoko, sending a message to an actor is a function call, but instead of blocking the caller until the call has returned, the message is enqueued on the callee, and a *future* representing that pending request immediately returned to the caller. The future is a placeholder for the eventual result of the request, that the caller can later query. Between issuing the request, and deciding to wait for the result, the caller is free to do other work, including issuing more requests to the same or other actors. Once the callee has processed the request, the future is completed and its result made available to the caller. If the caller is waiting on the future, its execution can resume with the result, otherwise the result is simply stored in the future for later use.

In Motoko, actors have dedicated syntax and types; messaging is handled by so called *shared* functions returning futures (shared because they are available to remote actors); a future, `f`, is a value of the special type `async T` for some type `T`; waiting on `f` to be completed is expressed using `await f` to obtain a value of type `T`. To avoid introducing shared state through messaging, for example, by sending an object or mutable array, the data that can be transmitted through shared functions is restricted to immutable, *shared* types.

To start, we consider the simplest stateful service: a `Counter` actor, the distributed version of our previous, local `counter` object.

## Example: a Counter service

Consider the following actor declaration:

``` motoko file=./examples/counter-actor.mo
```

<!---
actor Counter {

  var count = 0;

  public shared func inc() : async () { count += 1 };

  public shared func read() : async Nat { count };

  public shared func bump() : async Nat {
    count += 1;
    count;
  };
};
--->

The `Counter` actor declares one field and three public, *shared* functions:

-   the field `count` is mutable, initialized to zero and implicitly `private`.

-   function `inc()` asynchronously increments the counter and returns a future of type `async ()` for synchronization.

-   function `read()` asynchronously reads the counter value and returns a future of type `async Nat` containing its value.

-   function `bump()` asynchronously increments and reads the counter.

Shared functions, unlike local functions, are accessible to remote callers and have additional restrictions: their arguments and return value must be *shared* types - a subset of types that includes immutable data, actor references, and shared function references, but excludes references to local functions and mutable data. Because all interaction with actors is asynchronous, an actor’s functions must return futures, that is, types of the form `async T`, for some type `T`.

The only way to read or modify the state (`count`) of the `Counter` actor is through its shared functions.

A value of type `async T` is a future. The producer of the future completes the future when it returns a result, either a value or error.

Unlike objects and modules, actors can only expose functions, and these functions must be `shared`. For this reason, Motoko allows you to omit the `shared` modifier on public actor functions, allowing the more concise, but equivalent, actor declaration:

``` motoko name=counter file=./examples/counter-actor-sugar.mo
```

For now, the only place shared functions can be declared is in the body of an actor or actor class. Despite this restriction, shared functions are still first-class values in Motoko and can be passed as arguments or results, and stored in data structures.

The type of a shared function is specified using a shared function type. For example, the value `inc` has type `shared () → async Nat` and could be supplied as a standalone callback to some other service (see [publish-subscribe](sharing.md) for an example).

## Actor types

Just as objects have object types, actors have *actor types*. The `Counter` actor has the following type:

``` motoko no-repl
actor {
  inc  : shared () -> async ();
  read : shared () -> async Nat;
  bump : shared () -> async Nat;
}
```

Again, because the `shared` modifier is required on every member of an actor, Motoko both elides them on display, and allows you to omit them when authoring an actor type.

Thus the previous type can be expressed more succinctly as:

``` motoko no-repl
actor {
  inc  : () -> async ();
  read : () -> async Nat;
  bump : () -> async Nat;
}
```

Like object types, actor types support subtyping: an actor type is a subtype of a more general one that offers fewer functions with more general types.

## Using `await` to consume async futures

The caller of a shared function typically receives a future, a value of type `async T` for some T.

The only thing the caller, a consumer, can do with this future is wait for it to be completed by the producer, throw it away, or store it for later use.

To access the result of an `async` value, the receiver of the future use an `await` expression.

For example, to use the result of `Counter.read()` above, we can first bind the future to an identifier `a`, and then `await a` to retrieve the underlying `Nat`, `n`:

``` motoko include=counter
let a : async Nat = Counter.read();
let n : Nat = await a;
```

The first line immediately receives *a future of the counter value*, but does not wait for it, and thus cannot (yet) use it as a natural number.

The second line `await`s this future and extracts the result, a natural number. This line may suspend execution until the future has been completed.

Typically, one rolls the two steps into one and one just awaits an asynchronous call directly:

``` motoko include=counter
let n : Nat = await Counter.read();
```

Unlike a local function call, which blocks the caller until the callee has returned a result, a shared function call immediately returns a future, `f`, without blocking. Instead of blocking, a later call to `await f` suspends the current computation until `f` is complete. Once the future is completed (by the producer), execution of `await p` resumes with its result. If the result is a value, `await f` returns that value. Otherwise the result is some error, and `await f` propagates the error to the consumer of `await f`.

Awaiting a future a second time will just produce the same result, including re-throwing any error stored in the future. Suspension occurs even if the future is already complete; this ensures state changes and message sends prior to *every* `await` are committed.

:::danger

A function that does not `await` in its body is guaranteed to execute atomically - in particular, the environment cannot change the state of the actor while the function is executing. If a function performs an `await`, however, atomicity is no longer guaranteed. Between suspension and resumption around the `await`, the state of the enclosing actor may change due to concurrent processing of other incoming actor messages. It is the programmer’s responsibility to guard against non-synchronized state changes. A programmer may, however, rely on any state change prior to the await being committed.

:::

For example, the implementation of `bump()` above is guaranteed to increment and read the value of `count`, in one atomic step. The alternative implementation:

``` motoko no-repl
  public shared func bump() : async Nat {
    await inc();
    await read();
  };
```

does *not* have the same semantics and allows another client of the actor to interfere with its operation: each `await` suspends execution, allowing an interloper to change the state of the actor. By design, the explicit `await`s make the potential points of interference clear to the reader.

## Traps and Commit Points

A trap is a non-recoverable runtime failure caused by, for example, division-by-zero, out-of-bounds array indexing, numeric overflow, cycle exhaustion or assertion failure.

A shared function call that executes without executing an `await` expression never suspends and executes atomically. A shared function that contains no `await` expression is syntactically atomic.

An atomic shared function whose execution traps has no visible effect on the state of the enclosing actor or its environment - any state change is reverted, and any message that it has sent is revoked. In fact, all state changes and message sends are tentative during execution: they are committed only after a successful *commit point* is reached.

The points at which tentative state changes and message sends are irrevocably committed are:

-   implicit exit from a shared function by producing a result,

-   explict exit via `return` or `throw` expressions, and

-   explicit `await` expressions.

A trap will only revoke changes made since the last commit point. In particular, in a non-atomic function that does multiple awaits, a trap will only revoke changes attempted since the last await - all preceding effects will have been committed and cannot be undone.

For example, consider the following (contrived) stateful `Atomicity` actor:

``` motoko no-repl file=./examples/atomicity.mo
```

Calling (shared) function `atomic()` will fail with an error, since the last statement causes a trap. However, the trap leaves the mutable variable `s` with value `0`, not `1`, and variable `pinged` with value `false`, not `true`. This is because the trap happens *before* method `atomic` has executed an `await`, or exited with a result. Even though `atomic` calls `ping()`, `ping()` is tentative (queued) until the next commit point, so never delivered.

Calling (shared) function `nonAtomic()` will fail with an error, since the last statement causes a trap. However, the trap leaves the variable `s` with value `3`, not `0`, and variable `pinged` with value `true`, not `false`. This is because each `await` commits its preceding side-effects, including message sends. Even though `f` is complete by the second await on `f`, this await also forces a commit of the state, suspends execution and allows for interleaved processing of other messages to this actor.

## Query functions

In Internet Computer terminology, all three `Counter` functions are *update* messages that can alter the state of the canister when called. Effecting a state change requires agreement amongst the distributed replicas before the Internet Computer can commit the change and return a result. Reaching consensus is an expensive process with relatively high latency.

For the parts of applications that don’t require the guarantees of consensus, the Internet Computer supports more efficient *query* operations. These are able to read the state of a canister from a single replica, modify a snapshot during their execution and return a result, but cannot permanently alter the state or send further Internet Computer messages.

Motoko supports the implementation of Internet Computer queries using `query` functions. The `query` keyword modifies the declaration of a (shared) actor function so that it executes with non-committing, and faster, Internet Computer query semantics.

For example, we can extend the `Counter` actor with a fast-and-loose variant of the trustworthy `read` function, called `peek`:

``` motoko file=./examples/CounterWithQuery.mo
```

The `peek()` function might be used by a `Counter` frontend offering a quick, but less trustworthy, display of the current counter value.

It is a compile-time error for a query method to call an actor function since this would violate dynamic restrictions imposed by the Internet Computer. Calls to ordinary functions are permitted.

Query functions can be called from non-query functions. Because those nested calls require consensus, the efficiency gains of nested query calls will be modest at best.

The `query` modifier is reflected in the type of a query function:

``` motoko no-repl
  peek : shared query () -> async Nat
```

As before, in `query` declarations and actor types the `shared` keyword can be omitted.

## Messaging Restrictions

The Internet Computer places restrictions on when and how canisters are allowed to communicate. These restrictions are enforced dynamically on the Internet Computer but prevented statically in Motoko, ruling out a class of dynamic execution errors. Two examples are:

-   canister installation can execute code, but not send messages.

-   a canister query method cannot send messages.

These restrictions are surfaced in Motoko as restrictions on the context in which certain expressions can be used.

In Motoko, an expression occurs in an *asynchronous context* if it appears in the body of an `async` expression, which may be the body of a (shared or local) function or a stand-alone expression. The only exception are `query` functions, whose body is not considered to open an asynchronous context.

In Motoko calling a shared function is an error unless the function is called in an asynchronouus context. In addition, calling a shared function from an actor class constructor is also an error.

The `await` construct is only allowed in an asynchronous context.

The `async` construct is only allowed in an asynchronous context.

It is only possible to `throw` or `try/catch` errors in an asynchronous context. This is because structured error handling is supported for messaging errors only and, like messaging itself, confined to asynchronous contexts.

These rules also mean that local functions cannot, in general, directly call shared functions or `await` futures. This limitation can sometimes be awkward: we hope to extend the type system to be more permissive in future.

<!---
TODO: scoped awaits (if at all)
--->

## Actor classes generalize actors

An actor *class* generalizes a single actor declaration to the declaration of family of actors satisfying the same interface. An actor class declares a type, naming the interface of its actors, and a function that constructs a fresh actor of that type each time it is supplied with an argument. An actor class thus serves as a factory for manufacturing actors. Because canister installation is asynchronous on the Internet Computer, the constructor function is asynchronous too, and returns its actor in a future.

For example, we can generalize `Counter` given above to `Counter(init)` below, by introducing a constructor parameter, variable `init` of type `Nat`:

`Counters.mo`:

``` motoko name=Counters file=./examples/Counters.mo
```

If this class is stored in file `Counters.mo`, then we can import the file as a module and use it to create several actors with different initial values:

``` motoko include=Counters
import Counters "Counters";

let C1 = await Counters.Counter(1);
let C2 = await Counters.Counter(2);
(await C1.read(), await C2.read())
```

The last two lines above *instantiate* the actor class twice. The first invocation uses the initial value `1`, where the second uses initial value `2`. Because actor class instantiation is asynchronous, each call to `Counter(init)` returns a future that can be `await`ed for the resulting actor value. Both `C1` and `C2` have the same type, `Counters.Counter` and can be used interchangeably.

:::note

For now, the Motoko compiler gives an error when compiling programs that do not consist of a single actor or actor class. Compiled programs may still, however, reference imported actor classes. For more information, see [Importing actor classes](modules-and-imports.md#importing-actor-classes) and [Actor classes](actor-classes.md#actor-classes).

:::
