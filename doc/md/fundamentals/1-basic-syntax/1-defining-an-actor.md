---
sidebar_position: 1
hide_table_of_contents: true
---

# Defining an actor

In Motoko, an **actor**, declared with the `actor` keyword, is a computational process with its own  [state](https://internetcomputer.org/docs/motoko/fundamentals/state) and behavior.

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

This code defines an actor that can be deployed on ICP.
The actor is declared as 'persistent` so that its state, `count`, will be preserved
when the actor is upgraded.
<!---TODO update persistent link---->
Learn more about persistence: [link](https://internetcomputer.org/docs/motoko/icp-features/data-persistence)

Another actor can call `Main.greet()` with an argument and await the result:

```motoko include=Main
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
