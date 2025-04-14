---
sidebar_position: 1
hide_table_of_contents: true
---

# Defining an actor

In Motoko, an actor is a unit of computation that encapsulates [state](https://internetcomputer.org/docs/motoko/fundamentals/state) and behavior. Unlike traditional functions or objects in other programming languages, actors operate independently and interact with each other through [asynchronous](https://internetcomputer.org/docs/motoko/fundamentals/actors-async#async--await) messaging.

Actors are objects with special system-level capabilities. An actor supports asynchronous operations and can persist state across calls. Unlike modules, actors cannot be instantiated on their own using `actor()`. Each actor runs independently with its own message queue, allowing concurrent execution.

A Motoko program typically starts by defining an actor.

```motoko
// Declares an actor named Main
actor Main {
  // Defines a hello function that returns the message "Hello, world!"
  public query func hello() : async Text {
    "Hello, world!"
  };
};

// Actors in Motoko communicate asynchronously. await ensures the result is retrieved once the function completes.
await Main.hello();
```

A Motoko actor always presents its interface as a suite of named [functions](https://internetcomputer.org/docs/motoko/fundamentals/basic-syntax/functions) with defined argument and return types. When Motoko code is compiled, this interface is automatically generated in **[Candid](https://internetcomputer.org/docs/building-apps/interact-with-canisters/candid/candid-concepts)**, an interface description language.


## Resources

- [Actors](https://internetcomputer.org/docs/motoko/fundamentals/async-actors)

<img src="https://cdn-assets-eu.frontify.com/s3/frontify-enterprise-files-eu/eyJwYXRoIjoiZGZpbml0eVwvYWNjb3VudHNcLzAxXC80MDAwMzA0XC9wcm9qZWN0c1wvNFwvYXNzZXRzXC8zOFwvMTc2XC9jZGYwZTJlOTEyNDFlYzAzZTQ1YTVhZTc4OGQ0ZDk0MS0xNjA1MjIyMzU4LnBuZyJ9:dfinity:9Q2_9PEsbPqdJNAQ08DAwqOenwIo7A8_tCN4PSSWkAM?width=2400" alt="Logo" width="150" height="150" />
