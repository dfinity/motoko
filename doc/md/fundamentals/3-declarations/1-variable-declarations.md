---
sidebar_position: 1
---

# Variable declarations

In Motoko, variables are declared using:

- `let` for immutable bindings: Once assigned, the value cannot be changed.

- `var` for mutable bindings: The value can be updated later.

## Immutable variables

The `let` keyword is used to bind a name to the result of an expression, creating an immutable variable. When a let declaration is made, the expression on the right-hand side is evaluated once, and its resulting value is associated with the given name. This value cannot be changed after the initial assignment. The Motoko compiler enforces this immutability by generating a compile-time error if there is any attempt to modify the variable later in the code. This behavior helps ensure safety and predictability, especially in concurrent or distributed applications running on ICP.

```motoko no-repl
let x = 10;
x := 20; // Error: Cannot assign to immutable variable
```

Mutable variables declared with `var` do not support pattern matching. For example, the following is invalid:

```motoko no-repl
var (a, b) = (1, 2); // Not supported
```

## Mutable variables

A `var` declaration in Motoko defines a mutable variable whose value can be updated after it is initially assigned. Unlike `let`, which creates an immutable binding, `var` allows reassignment using the `:=` operator. This means that the variable can hold different values over time, making it suitable for scenarios where state changes are required.

```motoko no-repl
var y = 10;
y := 20; // Allowed, updates the value of y
```

`let` bindings do support pattern matching:

```motoko
let (a, b) = (1, 2); // Supported
```
