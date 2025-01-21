---
sidebar_position: 15
---

# Messaging restrictions



ICP places restrictions on when and how canisters are allowed to communicate. These restrictions are enforced dynamically but prevented statically in Motoko, ruling out a class of dynamic execution errors. Two examples are:

-   Canister installation can execute code, but not send messages.

-   A canister query method cannot send messages.

These ICP restrictions are surfaced in Motoko as constraints on the program context in which certain expressions can be appear.
Violations of these constraints are reported as errors by the type checker.

In Motoko, an expression occurs in an asynchronous context if it appears in the body of an `async` expression, which may be the body of a shared or local function or a stand-alone expression. The only exception are `query` functions, whose body is not considered to open an asynchronous context.

In Motoko calling a shared function is an error unless the function is called in an asynchronous context. Calling a shared function from an actor class constructor is also an error.

The `await` and `async` constructs are only allowed in an asynchronous context.

It is only possible to `throw` or `try/catch` errors in an asynchronous context. This is because structured error handling is supported for messaging errors only and, like messaging itself, confined to asynchronous contexts.

These rules also mean that local functions cannot directly call shared functions or `await` futures.

(The same restrictions apply to `await*` and `async*` expressions.)

<img src="https://github.com/user-attachments/assets/844ca364-4d71-42b3-aaec-4a6c3509ee2e" alt="Logo" width="150" height="150" />