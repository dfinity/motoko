---
sidebar_position: 5
---

# Async data



On ICP, communication between canisters is asynchronous. Sending a message together with a callback from one canister to another schedules a request in the receiver. Completion of the request triggers the callback to the sender, allowing the sender to process the result.

In Motoko, sending an ICP asynchronous message is abstracted as calling a shared function that returns an asynchronous result.
Like several other languages, Motoko offers `async` and `await` to support convenient programming with asynchronous functions and computations.

In Motoko, executing an asynchronous expression, whether a call to a shared function, or just a local `async` expression, produces a future, an object of type `async T`, for some result type `T`.
Instead of blocking the caller until the call has returned, the message is enqueued on the callee and the future representing that pending request is immediately returned to the caller. The future is a placeholder for the eventual result of the request that the caller can later query.

The syntax `await` synchronizes on a future, and suspends computation until the future is completed by its producer.

Between issuing the request and deciding to wait for the result, the caller is free to do other work. Once the callee has processed the request, the future is completed and its result made available to the caller. If the caller is waiting on the future, its execution can resume with the result, otherwise the result is simply stored in the future for later use.

The combination of `async`/`await` constructs simplifies asynchronous programming  by allowing `await`s to be embedded within ordinary sequential code, without requiring tricky management of asynchronous callbacks.

## Async functions

Here is an example program that uses async functions:

``` motoko file=../examples/counter-actor.mo
```

The `Counter` actor declares one field and three public, shared functions:

-   The field `count` is mutable, initialized to zero and implicitly `private`.

-   Function `inc()` asynchronously increments the counter and returns a future of type `async ()` for synchronization.

-   Function `read()` asynchronously reads the counter value and returns a future of type `async Nat` containing its value.

-   Function `bump()` asynchronously increments and reads the counter.


The only way to read or modify the state (`count`) of the `Counter` actor is through its shared functions.

## Using `await` to consume async futures

The caller of a shared function typically receives a future, a value of type `async T` for some T.

The only thing the caller can do with this future is wait for it to be completed by the producer, throw it away, or store it for later use.

To access the result of an `async` value, the receiver of the future uses an `await` expression.

For example, to use the result of `Counter.read()` above, we can first bind the future to an identifier `a`, and then `await a` to retrieve the underlying [`Nat`](../base/Nat.md), `n`:

``` motoko no-repl
let a : async Nat = Counter.read();
let n : Nat = await a;
```

The first line immediately receives a future of the counter value, but does not wait for it, and thus cannot use it as a natural number yet.

The second line `await`s this future and extracts the result, a natural number. This line may suspend execution until the future has been completed.

Typically, one rolls the two steps into one and just awaits an asynchronous call directly:

``` motoko no-repl
let n : Nat = await Counter.read();
```

Unlike a local function call, which blocks the caller until the callee has returned a result, a shared function call immediately returns a future, `f`, without blocking. Instead of blocking, a later call to `await f` suspends the current computation until `f` is complete. Once the future is completed (by the producer), execution of `await p` resumes with its result. If the result is a value, `await f` returns that value. Otherwise the result is some error, and `await f` propagates the error to the consumer of `await f`.

Awaiting a future a second time will just produce the same result, including re-throwing any error stored in the future. Suspension occurs even if the future is already complete; this ensures state changes and message sends prior to every `await` are committed.

:::danger

A function that does not `await` in its body is guaranteed to execute atomically. In particular, the environment cannot change the state of the actor while the function is executing. If a function performs an `await`, however, atomicity is no longer guaranteed. Between suspension and resumption around the `await`, the state of the enclosing actor may change due to concurrent processing of other incoming actor messages. It is the programmer’s responsibility to guard against non-synchronized state changes. A programmer may, however, rely on any state change prior to the await being committed.

:::

For example, the implementation of `bump()` above is guaranteed to increment and read the value of `count`, in one atomic step. The following alternative implementation does not have the same semantics and allows another client of the actor to interfere with its operation.

``` motoko no-repl
  public shared func bump() : async Nat {
    await inc();
    await read();
  };
```

Each `await` suspends execution, allowing an interloper to change the state of the actor. By design, the explicit `await`s make the potential points of interference clear to the reader.

## Mops packages for async data flow

- [`maf`](https://mops.one/maf) and [`mal`](https://mops.one/mal): Async data deliveries.

- [`rxmo`](https://mops.one/rxmo): A library for reactive programming using observables, making it easier to compose asynchronous or callback-based code.

<img src="https://github.com/user-attachments/assets/844ca364-4d71-42b3-aaec-4a6c3509ee2e" alt="Logo" width="150" height="150" />