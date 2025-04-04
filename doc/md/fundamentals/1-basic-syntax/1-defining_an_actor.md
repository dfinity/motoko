---
sidebar_position: 1
---

# Defining an actor

In Motoko, an actor is a unit of computation that encapsulates state and behavior. Unlike traditional functions or objects in other programming languages, actors operate independently and interact with each other through asynchronous messaging.

Actors are objects with special system-level capabilities. An actor supports asynchronous operations and can persist state across calls. Unlike modules, actors cannot be instantiated on their own using `actor()`. Each actor runs independently with its own message queue, allowing concurrent execution.

A Motoko program typically starts by defining an actor:

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

A Motoko actor always presents its interface as a suite of named [functions](/docs/motoko/fundamentals/basic-syntax/functions) with defined argument and return types. When Motoko code is compiled, this interface is automatically generated in **[Candid](https://internetcomputer.org/docs/building-apps/interact-with-canisters/candid/candid-concepts)**, an interface description language.


## References

- [Actors](/docs/motoko/fundamentals/async-actors)

<img src="https://github.com/user-attachments/assets/844ca364-4d71-42b3-aaec-4a6c3509ee2e" alt="Logo" width="150" height="150" />
