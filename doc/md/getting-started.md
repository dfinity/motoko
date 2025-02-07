---
sidebar_position: 2
---

# Getting Started

This section introduces the core concepts of Motoko, covering essential syntax, data types, and expressions. Before you begin writing canisters in Motoko, you should be familiar with a few of the basic concepts and terms specific to Motoko.

To learn through practical examples, visit the **[Motoko tutorial](https://internetcomputer.org/docs/current/motoko/tutorial/)**.  

### Traps

A trap is a runtime error that causes execution to abort immediately. Common causes include division by zero, out-of-bounds array access and pattern match failure

If a trap occurs inside an actor message, only that message fails—other messages continue execution.  

To trigger a trap manually, use `Debug.trap`:  

```motoko
import Debug "mo:base/Debug";

Debug.trap("oops!");
```

<img src="https://github.com/user-attachments/assets/844ca364-4d71-42b3-aaec-4a6c3509ee2e" alt="Logo" width="150" height="150" />
