---
sidebar_position: 14
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

Assertions help catch logic errors early, but should not be used for regular error handling.

<!---
Notes:
Insert link to regular error handling doc when available

Insert link to base library reference for Assertions
-->
