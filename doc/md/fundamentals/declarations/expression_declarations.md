# Expression declarations

An expression declaration is a statement where an expression is evaluated only for its side effects without assigning it to a variable. It does not introduce any new bindings.

## Rules for expression declarations

1. The expression must have a valid type `T`, and it evaluates normally.
2. If it appears in a sequence of declarations but is not the last declaration, then `T` must be `()` (i.e., it must not return a value).

## Basic usage

Expression declarations are often used for functions or operations that produce side effects, such as printing or modifying state.

```motoko
Debug.print("Hello, Motoko!");
```

- This prints `"Hello, Motoko!"` but does not assign it to a variable.

### Expression declarations in a sequence

If an expression is placed inside a sequence of declarations, but it is not the last declaration, it must return `()`.

```motoko
let x = 10;
Debug.print("Processing..."); // Expression declaration with side effects
let y = x * 2; // Valid, since Debug.print() returns ()
```

- `Debug.print()` is used for its side effect, and since it returns `()`, it can appear before `let y = x * 2;`.

### Invalid example

```motoko
let x = 10;
x * 2;  // This expression produces a value but is not assigned.
let y = 5;
```

- The expression `x * 2;` returns a value (`Nat`), but it is not assigned.
- Since it is not the last declaration, it must return `()`.
