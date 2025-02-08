---
sidebar_position: 13
---
# Traps

A trap is a runtime error that causes execution to abort immediately. Common causes include division by zero, out-of-bounds array access and pattern match failure

If a trap occurs inside an actor message, only that message fails—other messages continue execution.  

To trigger a trap manually, use `Debug.trap`:  

```motoko
import Debug "mo:base/Debug";

Debug.trap("oops!");
```

## Quick references

-[Traps](https://internetcomputer.org/docs/current/motoko/main/writing-motoko/actors-async/#traps)
