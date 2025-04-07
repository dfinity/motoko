---
sidebar_position: 12
---

# Traps

A trap is a [runtime error](/docs/motoko/fundamentals/error-handling) that causes execution to abort immediately. Common causes include division by zero, out-of-bounds array access, or [[pattern match](/docs/motoko/fundamentals/pattern-matching)] failure.

If a trap occurs during message execution, only that message fails. Other messages continue execution.

To trigger a trap manually, use `Debug.trap`.

```motoko
import Debug "mo:base/Debug";

Debug.trap("oops!");
```

## Assertions

An assertion checks a condition at runtime and traps if it fails.

```motoko
let n = 10;
assert n % 2 == 1; // Traps
```

```motoko
let n = 10;
assert n % 2 == 0; // Succeeds
```

Assertions help catch logic errors early, but should not be used for regular [error handling](/docs/motoko/fundamentals/error-handling).

<img src="https://github.com/user-attachments/assets/844ca364-4d71-42b3-aaec-4a6c3509ee2e" alt="Logo" width="150" height="150" />