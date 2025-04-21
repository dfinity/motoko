---
sidebar_position: 1
---

# Variable declarations

In Motoko, variables are declared using either `let` (immutable) or `var` (mutable).

| Feature                 | `let`    | `var`       |
|-------------------------|---------|-------------|
| Reassignment           | Not allowed | Allowed |
| Value modification     | Cannot be updated after assignment. | Can be updated after assignment. |

In `persistent` actors, the values of `let` and `var` declarations are automatically preserved across upgrades, unless explicitly marked as `transient`. In non-`persistent` actors, these values are not retained unless marked as `stable`.  
The `persistent` keyword simply treats all `let` and `var` declarations as `stable` by default.  
Motoko encourages a programming style that favours immutability (using `let` over `var`), aligning with functional programming principles.

## Immutable variables

A `let` declaration names the value of an expression so that the expression can be evaluated once and its result, a value, referenced by that name. The compiler ensures that any attempt to modify an immutable variable results in an error.

```motoko no-repl
let x = 10;
x := 20; // Error: Cannot assign to immutable variable
```

## Mutable variables

A `var` declaration refers to a variable whose value can be updated. Unlike `let`, `var` allows reassignment.

```motoko no-repl
var y = 10;
y := 20; // Allowed, updates the value of y
```

:::info
Mutable variables declared with `var` do not support pattern matching. For example, the following is invalid:

```motoko no-repl
var (a, b) = (1, 2); // Not supported
```

In contrast, `let` bindings do support pattern matching:

```motoko
let (a, b) = (1, 2); // Supported
```

:::
