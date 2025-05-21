---
sidebar_position: 1
---

# Variable declarations

In Motoko variables represent state within actors, classes, and modules. Variables are declared using either `let` (immutable) or `var` (mutable).

| Feature                 | `let`    | `var`       |
|-------------------------|---------|-------------|
| Reassignment           | Not allowed | Allowed |
| Value modification     | Cannot be updated after assignment. | Can be updated after assignment. |

:::info

In `persistent` actors, `let` and `var` bindings are treated as stable by default. Actor state must use [stable types](https://internetcomputer.org/docs/motoko/fundamentals/types/stable-types), otherwise, declarations must be marked `transient`.

The `persistent` keyword eliminates the need for explicit `stable` annotations, and `transient` replaces the legacy `flexible` modifier.

Motoko encourages a programming style that favours immutability (using `let` over `var`), reflecting functional programming principles.
:::

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

Mutable variables declared with `var` do not support pattern matching. For example, the following is invalid:

```motoko no-repl
var (a, b) = (1, 2); // Not supported
```

In contrast, `let` bindings do support pattern matching:

```motoko no-repl
let (a, b) = (1, 2); // Supported
```
