---
sidebar_position: 6
---

# Mutable state

| Feature         | Mutable [state](/docs/motoko/fundamentals/state)                  | Immutable [state](/docs/motoko/fundamentals/state)               |
|----------------|--------------------------------|--------------------------------|
| Accessibility  | Private to the [actor](/docs/motoko/fundamentals/actors-async) that owns it. | Can be shared between actors. |
| Modification   | Can be modified internally. | Cannot be modified after creation. |
| Sharing        | Cannot be directly shared. | Can be safely passed between actors. |
| Concurrency    | Prevents race conditions by keeping [state](/docs/motoko/fundamentals/state) local. | Safe for remote calls. |

Each [actor](/docs/motoko/fundamentals/actors-async) in Motoko may use mutable [state](/docs/motoko/fundamentals/state), but it cannot share it directly with other actors. This ensures that state modifications remain isolated, preventing race conditions and unintended side effects in concurrent execution.

Immutable data, on the other hand, can be shared between actors. Actors can also interact with each other's external entry points, which serve as shareable functions for communication.

## Why mutable state is private

A key design principle in Motoko is that mutable [state](/docs/motoko/fundamentals/state) is always private to the [actor](/docs/motoko/fundamentals/actors-async) that allocates it. Unlike shareable data, mutable state cannot be accessed remotely. This prevents concurrency conflicts, ensuring that only the actor responsible for the data can modify it.

Motoko enforces strict control over mutable state, ensuring that concurrent execution remains predictable and error-free.

The following actor maintains a private mutable counter that can only be modified through its own functions:

```motoko no-repl
actor Counter {
    stable var count: Nat = 0; // Private mutable state

    public shared func increment(): async Nat {
        count += 1;
        return count;
    };

    public query func getCount(): async Nat {
        return count;
    };
};
```

Since `count` is mutable, it can be modified internally but cannot be accessed directly from outside the [actor](/docs/motoko/fundamentals/actors-async). Instead, other actors must use the provided entry points to retrieve or update its value.


<img src="https://github.com/user-attachments/assets/844ca364-4d71-42b3-aaec-4a6c3509ee2e" alt="Logo" width="150" height="150" />