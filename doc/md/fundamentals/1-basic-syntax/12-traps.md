---
sidebar_position: 12
hide_table_of_contents: true
---

# Assertions

An assertion checks a condition at runtime and traps if it fails.

```motoko
let n = 10;
assert n % 2 == 1; // Traps
```

```motoko
let n = 10;
assert n % 2 == 0; // Succeeds
```

Assertions help catch logic errors early, but should not be used for regular [error handling](../../fundamentals/9-error-handling.md).

