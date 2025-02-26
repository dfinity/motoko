---
sidebar_position: 2
---

# Options

Options provide a structured way to represent values that may or may not be present. Instead of using `null` directly, the option type enforces explicit handling, making programs safer and reducing unexpected failures.

## Checking for presence

To determine if an option contains a value, `Option.isSome` returns `true` if it is not `null`.

```motoko norepl
import Option "mo:base/Option";

let value: ?Nat = ?5;
if (Option.isSome(value)) {
    Debug.print("Value is present.");
}
```

By leveraging the `Option` module, handling optional values becomes more concise and expressive, reducing the need for explicit `switch` statements.

## Providing default values

Instead of manually handling `null` cases with pattern matching, `Option.get` allows for cleaner fallback logic:

```motoko norepl
import Option "mo:base/Option";

let username: ?Text = null;
let displayName = Option.get(username, "Guest"); // "Guest" if username is null
```

This approach ensures that missing values are safely replaced with a default.

## Using options for error handling

Options can be used to catch expected failures instead of calling `trap`, making a function return `null` when it encounters an invalid input.

```motoko norepl
func safeDivide(a: Int, b: Int): ?Int {
    if (b == 0) null else ?(a / b);
}

let result1 = safeDivide(10, 2); // ?5
let result2 = safeDivide(10, 0); // null
```

This prevents division errors from interrupting program execution.

## Applying transformations to options

The `Option.map` function enables applying a transformation only if the value is present.

```motoko norepl
import Option "mo:base/Option";

  let number: ?Nat = ?10;
  let doubled = Option.map<Nat, Nat>(number, func (x : Nat) = x * 2); // ?20
```

If `number` is `null`, `map` ensures the result remains `null` instead of performing an invalid operation.

## Applying an optional function

Sometimes, both the function and value are optional. `Option.apply` allows calling a function only if both are present.

```motoko norepl
import Option "mo:base/Option";

let maybeIncrement: ?(Nat -> Nat) = ?(func x: Nat = x + 1);
let maybeValue: ?Nat = ?10;

let result = Option.apply<Nat,Nat>(maybeValue, maybeIncrement); // ?11
```

If either `maybeFunction` or `maybeValue` is `null`, the result remains `null`.

This is useful when chaining optional operations that may return `null`.

## Combining multiple optional values

When working with multiple optional values, using `Option.chain` allows safely processing them without unnecessary `switch` statements.

```motoko norepl
import Option "mo:base/Option";

let firstName: ?Text = ?"Alice";
let lastName: ?Text = ?"Smith";

func combineNames(f: Text, l: Text): Text {
    f # " " # l;
};

let fullName = Option.chain<Text, Text>(firstName, func (f: Text) {
    Option.map<Text, Text>(lastName, func (l: Text) { combineNames(f, l) });
});

// ?("Alice Smith")
```

If either `firstName` or `lastName` is `null`, the result remains `null`.

## References

- [Option](https://internetcomputer.org/docs/current/motoko/main/base/option)
