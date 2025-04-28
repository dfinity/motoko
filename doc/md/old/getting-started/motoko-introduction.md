---
sidebar_position: 1
---

# Meet Motoko: The language shaping the future of Web3

<img src="https://github.com/user-attachments/assets/844ca364-4d71-42b3-aaec-4a6c3509ee2e" alt="Logo" width="150" height="150" />

Motoko is a modern, general-purpose programming language you can use specifically to author ICP canister smart contracts. Although aimed primarily at ICP canister development, its design is general enough to support future compilation to other targets.

Motoko is designed to be approachable for programmers who have some basic familiarity with object-oriented and/or functional programming idioms in either JavaScript, or another modern programming language, such as Rust, Swift, TypeScript, C#, or Java.


```motoko
// A simple Motoko smart contract.

actor Main {
  public query func hello() : async Text {
    "Hello, world!"
  };
};

await Main.hello();
```


Motoko provides:

-   A high-level language for programming applications to run on ICP.

-   A simple design that uses familiar syntax that is easy for programmers to learn.

-   An **actor-based** programming model optimized for efficient message handling.

-   **Orthogonal persistence** for simple, safe, and efficient data storage without a database or secondary memory.

-   An interpreter and compiler that you can use to test and compile the WebAssembly code for autonomous applications.

# Why Motoko?

ICP supports programs written in different languages. The only requirement is that the program must support compilation to WebAssembly code. WebAssembly is a low-level computer instruction format for virtual machines. Because WebAssembly code is designed to provide portable low-level instructions that enable applications to be deployed on many platforms such as the web, it is a natural fit for deploying applications that are intended to run on ICP. However, most of the higher-level languages that support compiling to WebAssembly are either unsafe or complex for developers who want to deliver secure applications without a long learning curve.

To address the need for correctness without complexity, DFINITY has designed its own Motoko programming language. Motoko provides a simple and expressive alternative to other programming languages that is easy to learn whether you are a new or experienced programmer.

## Motivation and goals

Motoko strives to be a simple and useful language for ICP smart contracts that uses a familiar syntax that incorporates the actor model. Motoko offers seamless integration for ICP features and makes the most out of WebAssembly's present and future functionalities.

Motoko is not, and is not intended to be, the only language for implementing canister smart contracts. Canister development kits exist for Rust, TypeScript, Python, and soon Solidity. ICP's goal is to enable any language to be able to produce canister smart contracts by compiling the canister source code into WebAssembly.

However, its tailored design means Motoko should be the easiest and safest language for coding on ICP.

## Key design points

Motoko provides many other developer productivity features. It has derived inspiration for its design from languages such as Java, JavaScript, C#, Swift, Pony, ML, and Haskell.

Motoko syntax should be familiar to JavaScript programmers, but without the quirks of JavaScript.

Motoko is a concurrent, actor-oriented language with convenient support for concurrency control using async/await and futures.
Its core is a call-by-value, impure functional language with imperative and object-oriented features.

Motoko is strongly typed and offers option types, records, variants, generics and subtyping. The type system is structural so that types with different names but equivalent definition are interchangeable; subtyping allows values to be used at their precise type but also at any more general supertype.

- Motoko is safety-oriented.
- Memory management is automated using a garbage collector.
- Implicit null values are avoided. Nulls must be handled explicitly using option types.
- The type system cannot be circumvented using unsafe casts.
- Arithmetic either uses unbounded integers that cannot overflow, or, when using bounded integers, employs checked arithmetic that faults on overflow.
- All coercions between unrelated types are explicit.
- Implicit promotion of a value to another type can only be achieved by subtyping, an operation with zero cost.
- Motoko's support for concise, compiler-checked pattern matching encourages good coding style and auditable code.
- Error-prone, object-oriented inheritance is not supported.

Motoko canisters are compiled, not interpreted, producing small binaries with good performance.

### Native canister smart contract support

Motoko has native support for canister smart contracts, which are expressed as a Motoko actor. An actor is an autonomous object that fully encapsulates its state and communicates with other actors only through asynchronous messages.

### Code sequentially in direct style

On ICP, canisters can communicate with other canisters by sending asynchronous messages.

Asynchronous programming is hard, so Motoko enables you to author asynchronous code in much simpler, sequential style. Asynchronous messages are function calls that return a future, and the `await` construct allows you to suspend execution until a future has completed.

### Modern type system

Motoko has been designed to be intuitive to those familiar with JavaScript and other popular languages, but offers modern features such as sound structural types, generics, variant types, and statically checked pattern matching.

### Autogenerated IDL files

A Motoko actor always presents a typed interface to its clients as a suite of named functions with argument and future result types.

The Motoko compiler and IC SDK can emit this interface in a language neutral format called Candid, so other canisters, browser resident code and smart phone apps that support Candid can use the actor’s services. The Motoko compiler can consume and produce Candid files, allowing Motoko to seamlessly interact with canisters implemented in other programming languages, provided they also support Candid.

### Orthogonal persistence

ICP persists the memory and other state information of a canister as it is executed. Thus the state of a Motoko actor, including its in-memory data structures, survive indefinitely. Actor state does not need to be explicitly restored and saved to external storage.

### Upgrades

Motoko provides numerous features to help you leverage orthogonal persistence, including language features that allow you to retain a canister’s data as you upgrade the code of the canister.

For example, Motoko lets you declare certain variables as `stable`. The values of `stable` variables are automatically preserved across canister upgrades.

Once upgraded, the new interface is compatible with the previous one, meaning existing clients referencing the canister will continue to work, but new clients will be able to exploit its upgraded functionality. For more complex changes of any kind, Motoko provides a safe way of user-defined [explicit migration](../canister-maintenance/compatibility.md#explicit-migration).

## Getting started

Get started with Motoko by [setting up your developer environment](dev-env.md) and creating a simple [Hello, world!](quickstart.md) program.