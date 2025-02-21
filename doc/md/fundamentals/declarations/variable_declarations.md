---
sidebar_position: 1
---

---
sidebar_position: 1
---

# Variable declarations

In Motoko, variables are declared using either `let` (immutable) or `var` (mutable). Understanding the difference between immutability and mutability is crucial when writing efficient and predictable code.

## Immutable variables

A `let` declaration binds a value to a name and cannot be changed after assignment. The compiler ensures that any attempt to modify an immutable variable results in an error.

```motoko
let x = 10;
x := 20; // Error: Cannot assign to immutable variable
```

### Memory binding in `let`

When using `let`, Motoko stores the value directly in memory and does not allow it to change. Since it never changes, the compiler can perform optimizations, making `let` more efficient.

## Mutable variables

A `var` declaration allocates a space in memory where the value can be updated. Unlike `let`, `var` allows reassignment.

```motoko
var y = 10;
y := 20; // Allowed, updates the value of y
```

### Memory binding in `var`

When using `var`, Motoko creates a reference to a memory location. This reference allows the value at that location to be updated.

## `let` vs `var` comparison  

| Feature                 | `let`    | `var`       |
|-------------------------|---------|-------------|
| Reassignment           | Not allowed | Allowed |
| Value modification     | Fixed after declaration | Can be updated after assignment |
| Memory allocation      | Directly stores value | Stores a reference to a memory location |
| Persistence in actors  | Retained unless assigned a mutable value | Allowed with `stable var` |
| Upgrade behavior       | Persists unless holding a `var` reference | Must be explicitly marked `stable` to persist |
| Compiler optimization  | More optimizable | Less optimizable due to mutations |
