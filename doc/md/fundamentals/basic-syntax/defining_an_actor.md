---
sidebar_position: 1
---
# Defining an actor

In Motoko, an actor is a unit of computation that encapsulates state and behavior. Unlike traditional functions or objects in other programming languages, actors operate independently and interact with each other through asynchronous messaging.

A Motoko program typically starts by defining an actor. The example below declares an actor named `Main` with a `hello` function that returns a `"Hello, world!"` message:

```motoko
actor Main {
  public query func hello() : async Text {
    "Hello, world!"
  };
};

await Main.hello();
```

A Motoko actor always presents its interface as a suite of named functions with defined argument types and return types. The Motoko compiler and IC SDK generates this interface in **Candid**, a language-neutral format that enables communication between different systems.

Since actors in Motoko communicate asynchronously, `await` ensures the result is retrieved once the function completes.
