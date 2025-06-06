---
sidebar_position: 28
---

# Abstracting asynchronous code


Functions are an abstraction mechanism, allowing you to name a computation and re-use that computation in different locations within your code simply by invoking the name of that function. When the function takes parameters, you can tailor the computation to different call sites by providing different arguments.

Programmers often improve their code by re-factoring common patterns of code into
a single, reusable function.

In Motoko, you might want to refactor code that involves asynchronous operations such as sending messages or awaiting futures.
Motoko's type system prevents you from using an ordinary function for this because ordinary functions are not allowed to send messages nor await.
You can, however, define a local, asynchronous function containing the asynchronous code, and then replace all occurrences of the pattern by a call to that function. Since these calls return futures, each call must be enclosed in an `await` to extract its future's result.

Though this can work, it has some overhead and pitfalls:
- Each call of the function involves sending an additional message to the actor itself.

- Every call must be awaited, significantly adding to the cost of the code it abstracts.

- Each await involves suspending the execution of the awaiter until a reply is available, allowing more interleavings, and thus more interference, with the execution of other concurrent messages.

Consider the following code that does some logging to a remote canister.

``` motoko
persistent actor class (Logger : actor { log : Text -> async () }) {

  var logging = true;

  func doStuff() : async () {
    // do stuff
    if (logging) { await Logger.log("stuff") };
    // do more stuff
    if (logging) { await Logger.log("more stuff") };
  }
}
```

To avoid repetition of the logging logic, it would be nice to refactor this code to use a helper function `maybeLog`.
The `maybeLog` function needs to be asynchronous because communicating with the `Logger` canister involves sending a message.


``` motoko
persistent actor class (Logger : actor { log : Text -> async () }) {

  var logging = true;

  func maybeLog(msg : Text) : async () {
    if (logging) { await Logger.log(msg) };
  };

  func doStuff() : async () {
    // do stuff
    await maybeLog("stuff");
    // do more stuff
    await maybeLog("more stuff");
  }
}
```

While this typechecks and runs, the code for `doStuff()` is now much less efficient than the original code, since each call to `maybeLog` function involves an additional `await` that suspends the execution of `doStuff()`, even when the `logging` flag is `false`.
The semantics of this code is also slightly different, since the value of the logging variable could, in principle, change between the call to `maybeLog` and the execution of its body, depending on the rest of the actor code.

A safer refactoring passes the current state of the `logging` variable with each call:


``` motoko
persistent actor class (Logger : actor { log : Text -> async () }) {

  var logging = true;

  func maybeLog(log : Bool, msg : Text) : async () {
    if (log) { await Logger.log(msg) };
  };

  func doStuff() : async () {
    // do stuff
    await maybeLog(logging, "stuff");
    // do more stuff
    await maybeLog(logging, "more stuff");
  }
}
```
## Computation types

To avoid the overhead and dangers of additional awaits, Motoko offers computation types, `async* T`, that, like future types, `async T`, can abstract asynchronous tasks.

Just as an `async` expression is used to create a future (by scheduling the execution of its body), an `async*` expression is used to create a computation (by delaying the execution of its body).
Similar to how `await` is used to consume the result of a future, `await*` is used to produce the result of a computation (by demanding another execution of its body).

From a typing perspective, futures and computations are very similar. Where they differ is in their dynamic behavior: a future is a stateful object that holds the result of a scheduled, asynchronous task while a computation is just an inert value describing a task.

Unlike `await` on a future, `await*` on a computation does not suspend the awaiter, it just immediately executes the computation much like an ordinary function call.
This means that awaiting an `async*` value only suspends its execution (to complete asynchronously), if the body of the `async*` does a proper `await`.
The `*` on these expressions is meant to indicate that the computation may involve 0 or more ordinary `await` expressions, and thus may be interleaved with the execution of other messages.

To create an `async*` value, you can just use an `async*` expression, but more typically, you'll declare a local function that returns an `async*` type.

To compute the result of an `async*` computation, you just use an `await*`.

Here's how we can refactor our original class to be clearer, efficient and have the same meaning, using computations instead of futures:

``` motoko
persistent actor class (Logger : actor { log : Text -> async () }) {

  var logging = true;

  func maybeLog(msg : Text) : async* () {
    if (logging) { await Logger.log(msg) };
  };

  func doStuff() : async () {
    // do stuff
    await* maybeLog("stuff");
    // do more stuff
    await* maybeLog("more stuff");
  }
}
```

One notable difference between `async` and `async*` expressions is that the former are eager, while the latter are not.
This means that calling the async version of `maybeLog` will eagerly schedule its body to run, even if the `async` result (a future) of the call is never `await`ed.
Awaiting the same future another time will always produce the original result: the message is executed just once.

On the other hand, calling the `async*` version of `maybeLog` will do nothing unless the result is `await*`-ed, and `await*`-ing the same computation several times will repeat
the computation each time.

For another example, suppose we define a
clap function with the side-effect of printing "clap".

``` motoko no-repl
import Debug "mo:base/Debug"
func clap() { Debug.print("clap") }
```

Now, using futures, this code will clap once:

``` motoko no-repl
let future = async { clap() };
```

This remains the case, no matter how often you await `future`.
For example:

``` motoko no-repl
let future = async { clap() };
await future;
await future;
```

Using computations, on the other hand, the following definition has no effect on its own:

``` motoko no-repl
let computation = async* { clap() };
```
But, the following example will clap twice:

``` motoko no-repl
let computation = async* { clap() };
await* computation;
await* computation;
```



:::danger

You should use `async*`/`await*` with care. An ordinary `await` is a commit point in Motoko: all your state changes will be committed at the `await` before suspension.
An `await*`, on the other hand, is not a commit point (since its body may not await at all, or commit at some indefinite point).
This means that traps within the awaited computation may roll back the state of the actor to the last commit point *before* the `await*`, not to the state at the `await*` itself.

:::

See the language manual for more details on the [`async*` type](../reference/language-manual#async-type-1), the [`async*` expression](../reference/language-manual#async-1) and the
[`await*` expression](../reference/language-manual#await-1).


## Mops packages for computations

- [`star`](https://mops.one/star): Used for handling asynchronous behavior and traps using `async*` functions.

<img src="https://github.com/user-attachments/assets/844ca364-4d71-42b3-aaec-4a6c3509ee2e" alt="Logo" width="150" height="150" />
