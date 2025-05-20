---
sidebar_position: 1
hide_table_of_contents: true
---

# Defining an actor

In Motoko, an **actor** is a computational process with its own [state](https://internetcomputer.org/docs/motoko/fundamentals/state) and behavior. Actors are declared with the `actor` keyword.

Unlike traditional functions or objects in other programming languages, actors operate independently and communicate via [asynchronous](https://internetcomputer.org/docs/motoko/fundamentals/actors-async#async--await) messaging. Each actor maintains its own message queue, enabling concurrent execution.

An actor's state is defined by its private variables, while its behavior is defined by the public functions it exposes to other actors.

You should define an actor when you want to encapsulate state and expose a public API that can be accessed asynchronously by other actors, canisters, or external clients.

More specifically, define an actor when:

- You are building a canister smart contract that maintains private state and exposes public functions.
- You want to create an application that runs on the Internet Computer and is accessible by users or other canisters.
- You want to take advantage of the actor model's benefits, such as memory isolation, single-threaded execution for update calls (avoiding race conditions), and asynchronous communication.

In Motoko, actors are defined at the top level of a source file using the `actor` keyword. Public functions within an actor must be marked `shared` and return `async` types to support remote, asynchronous calls.

An actor definition is required for a Motoko program to be deployed as a canister on ICP.

The state is defined by the actor's private variables and the behavior is defined by its public functions - the functions it exposes to other actors.

An actor operates independently but can communicate with other actors by calling their public functions. A call to a public function sends an [asynchronous](https://internetcomputer.org/docs/motoko/fundamentals/actors-async#async--await) message to the receiving actor.

Each actor maintains its own queues of incoming messages, one queue per sender. Message are processed in order, so that the processing of one message cannot interfere with the processing of another, protecting the actor's state from concurrent modification.

Because each actor processes its messages independently, multiple actors can execute messages at the same time, allowing for parallel execution at the level of actors.

Although a Motoko actor can have many internal components, including functions, classes and modules, the unit of deployment on ICP is always an actor. If you want to deploy Motoko code as an  ICP canister that code must define an actor.

Every Motoko actor corresponds to an ICP canister. Conversely, any ICP canister, regardless of the implementation language is represented as an actor when imported into Motoko.

```motoko name=Main
// Declares an actor named Main.
persistent actor Main {
  // Define a private variable called 'count' to track the number of greetings.
  var count : Nat = 0;

  // Define a public function that asynchronously returns a greeting
  // and increments the counter.
  public func greet(name : Text) : async Text {
    count += 1;
    "Hello, " # name # "! You are visitor number " # debug_show(count);
  };

  // Define a publically called function to 
  // return the current value of 'count' separately.
  public query func readCount() : async Nat {
    count
  };
};
```

:::info
This code defines an actor that can be deployed on ICP.
The actor is declared as `persistent` so that its state, `count`, will be preserved
when the actor is upgraded.
<!---TODO update persistence link?---->
Learn more about [persistence](https://internetcomputer.org/docs/motoko/icp-features/data-persistence).
:::

Another actor can call `Main.greet()` with an argument and await the result:

```motoko include=Main
await Main.greet("Programmer");
```

A Motoko actor always presents its interface as a suite of named [functions](https://internetcomputer.org/docs/motoko/fundamentals/basic-syntax/functions) (also called methods) with defined argument and return types. When Motoko code is compiled, this interface is automatically translated to [Candid](https://internetcomputer.org/docs/building-apps/interact-with-canisters/candid/candid-concepts), an interface description language. The Candid description can be consumed by other canisters, including canisters written in another language such as Rust.

The above example's corresponding Candid interface can be found below.

```did
service : {
  greet : (text) -> (text);
  readCount : () -> (nat) query;
}
```

## Resources

- [Actors](https://internetcomputer.org/docs/motoko/fundamentals/async-actors)

<img src="https://cdn-assets-eu.frontify.com/s3/frontify-enterprise-files-eu/eyJwYXRoIjoiZGZpbml0eVwvYWNjb3VudHNcLzAxXC80MDAwMzA0XC9wcm9qZWN0c1wvNFwvYXNzZXRzXC8zOFwvMTc2XC9jZGYwZTJlOTEyNDFlYzAzZTQ1YTVhZTc4OGQ0ZDk0MS0xNjA1MjIyMzU4LnBuZyJ9:dfinity:9Q2_9PEsbPqdJNAQ08DAwqOenwIo7A8_tCN4PSSWkAM?width=2400" alt="Logo" width="150" height="150" />
