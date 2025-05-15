---
sidebar_position: 1
hide_table_of_contents: true
---

# Defining an actor

In Motoko, an `actor` is a unit of computation that encapsulates [state](https://internetcomputer.org/docs/motoko/fundamentals/state) and behavior. Unlike traditional functions or objects in other programming languages, actors operate independently and communicate via [asynchronous](https://internetcomputer.org/docs/motoko/fundamentals/actors-async#async--await) messaging. Each actor maintains its own message queue, enabling concurrent execution.

You should define an actor when you want to encapsulate state and expose a public API that can be accessed asynchronously by other actors, canisters, or external clients.

More specifically, define an actor when:

- You want to deploy your program on ICP.
- You want to take advantage of the actor model's benefits, such as memory isolation, single-threaded execution for update calls (avoiding race conditions), and asynchronous communication.

In Motoko, actors are defined at the top level of a source file using the `actor` keyword. Public functions within an actor must be marked `shared` and return `async` types to support remote, asynchronous calls.

An actor definition is required for a Motoko program to be deployed as a canister on ICP.

```motoko
// Declares an actor named Main.
// The actor is defined as 'persistent,' meaning its state will persist. 
// Learn more about persistence: [link]
persistent actor Main {
  // Define a variable called 'count' to track the number of greetings.
  var count : Nat = 0;

  // Define a publically callable function that when called 
  // will greet someone and increment the counter.
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

// Make a call to the greet function with the input of "Programmer"
await Main.greet("Programmer");
```

A Motoko actor always presents its interface as a suite of named [functions](https://internetcomputer.org/docs/motoko/fundamentals/basic-syntax/functions) (also called methods) with defined argument and return types. When Motoko code is compiled, this interface is automatically translated to [Candid](https://internetcomputer.org/docs/building-apps/interact-with-canisters/candid/candid-concepts), an interface description language. The Candid description can be consumed by other canisters, including canisters written in another language such as Rust.

The above example's corresponding Candid interface can be found below.

```did
service : {
  greet : (text) -> (text) query;
}
```

## Resources

- [Actors](https://internetcomputer.org/docs/motoko/fundamentals/async-actors)

<img src="https://cdn-assets-eu.frontify.com/s3/frontify-enterprise-files-eu/eyJwYXRoIjoiZGZpbml0eVwvYWNjb3VudHNcLzAxXC80MDAwMzA0XC9wcm9qZWN0c1wvNFwvYXNzZXRzXC8zOFwvMTc2XC9jZGYwZTJlOTEyNDFlYzAzZTQ1YTVhZTc4OGQ0ZDk0MS0xNjA1MjIyMzU4LnBuZyJ9:dfinity:9Q2_9PEsbPqdJNAQ08DAwqOenwIo7A8_tCN4PSSWkAM?width=2400" alt="Logo" width="150" height="150" />
