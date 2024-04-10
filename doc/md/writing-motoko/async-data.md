# Async data

## Functions and futures

In Motoko, sending a message to an actor is a **function call**, but instead of blocking the caller until the call has returned, the message is enqueued on the callee, and a **future** representing that pending request is immediately returned to the caller. The future is a placeholder for the eventual result of the request that the caller can later query.

Between issuing the request and deciding to wait for the result, the caller is free to do other work, including issuing more requests to the same or other actors. Once the callee has processed the request, the future is completed and its result made available to the caller. If the caller is waiting on the future, its execution can resume with the result, otherwise the result is simply stored in the future for later use.

In Motoko, actors have dedicated syntax and types:

- Messaging is handled by so called **shared** functions returning futures (shared because they are available to remote actors).

- Future, `f`, is a value of the special type `async T` for some type `T`.

- Waiting on `f` to be completed is expressed using `await f` to obtain a value of type `T`. To avoid introducing shared state through messaging, for example, by sending an object or mutable array, the data that can be transmitted through shared functions is restricted to immutable, **shared** types.


``` motoko file=../examples/counter-actor.mo
```

<!--
actor Counter {

  var count = 0;

  public shared func inc() : async () { count += 1 };

  public shared func read() : async Nat { count };

  public shared func bump() : async Nat {
    count += 1;
    count;
  };
};
-->



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

### Support for *asynchronous* behavior

In an *asynchronous* computing setting, a program and its running environment are permitted to perform *internal computations* that occur *concurrently* with one another.

Specifically, asynchronous programs are ones where the program’s requests of its environment do not (necessarily) require the program to wait for the environment. In the meantime, the program is permitted to make internal progress within this environment while the environment proceeds to complete the request. In the example, above, the program issues the second request before waiting for the first request to complete.

Symmetrically, the environment’s requests of the program do not (necessarily) require the environment to wait for the program’s answer: the environment can make external progress while the answer is produced.

We do not show an example of this “notify” pattern above, since it uses callbacks (and *higher-order* functions and control flow) and is thus more complex.

### Syntactic forms `async` and `await`

To address the need for clarity and simplicity, Motoko adopts the increasingly-common program constructs `async` and `await`, which afford the programmer a *structured* language for describing potentially-complex asynchronous dependency graphs.

The [async](language-manual.md#async) syntax introduces futures. A future value represents a *promise* of a result *that will be delivered, asynchronously, sometime in the future* (not shown in the first example above). You’ll learn more about futures when we introduce actors in [Actors and async data](actors-async.md).

Here, we merely use the ones that arise from calling `service1.computeAnswer(params)` and `service2.computeAnswer(params)`.

The syntax `await` synchronizes on a future, and suspends computation until the future is completed by its producer. We see two uses of `await` in the example above, to obtain the results from two calls to services.

When the developer uses these keywords, the compiler transforms the program as necessary, often doing complex transformations to the program’s control- and data-flow that would be tedious to perform by hand in a purely synchronous language. Meanwhile, the type system of Motoko enforces certain correct usage patterns for these constructs, including that types flowing between consumers and producers always agree, and that the types of data sent among services are permitted to flow there, and do not (for example) contain [private mutable state](mutable-state.md).


Here is a more complex example:

```motoko
actor Counter {

  var count = 0;

  public shared func inc() : async () { count += 1 };

  public shared func read() : async Nat { count };

  public shared func bump() : async Nat {
    count += 1;
    count;
  };
};
```

The `Counter` actor declares one field and three public, **shared** functions:

-   The field `count` is mutable, initialized to zero and implicitly `private`.

-   Function `inc()` asynchronously increments the counter and returns a future of type `async ()` for synchronization.

-   Function `read()` asynchronously reads the counter value and returns a future of type `async Nat` containing its value.

-   Function `bump()` asynchronously increments and reads the counter.

Shared functions, unlike local functions, are accessible to remote callers and have additional restrictions: their arguments and return value must be *shared* types - a subset of types that includes immutable data, actor references, and shared function references, but excludes references to local functions and mutable data. Because all interaction with actors is asynchronous, an actor’s functions must return futures, that is, types of the form `async T`, for some type `T`.

The only way to read or modify the state (`count`) of the `Counter` actor is through its shared functions.

A value of type `async T` is a future. The producer of the future completes the future when it returns a result, either a value or error.

Unlike objects and modules, actors can only expose functions, and these functions must be `shared`. For this reason, Motoko allows you to omit the `shared` modifier on public actor functions, allowing the more concise, but equivalent, actor declaration:

``` motoko name=counter file=./examples/counter-actor-sugar.mo
```

For now, the only place shared functions can be declared is in the body of an actor or actor class. Despite this restriction, shared functions are still first-class values in Motoko and can be passed as arguments or results, and stored in data structures.

The type of a shared function is specified using a shared function type. For example, the value `inc` has type `shared () → async Nat` and could be supplied as a standalone callback to some other service (see [publish-subscribe](sharing.md) for an example).