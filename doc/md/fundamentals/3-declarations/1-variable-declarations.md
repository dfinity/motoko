---
sidebar_position: 1
---

# Variable declarations

In Motoko, variables are declared using either `let` (immutable) or `var` (mutable).

| Feature                 | `let`    | `var`       |
|-------------------------|---------|-------------|
| Reassignment           | Not allowed | Allowed |
| Value modification     | Cannot be updated after assignment. | Can be updated after assignment. |

| Persistence in actors  | Retained unless assigned a mutable value. | Allowed with `stable var`. |
| Upgrade behavior       | Persists unless holding a `var` reference. | Must be explicitly marked `stable` to persist. |
| Compiler optimization  | More optimal | Less optimal |

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

