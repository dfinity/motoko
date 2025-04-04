---
sidebar_position: 10
---

# Options and results

| Type    | Syntax                  | Purpose                          | When to use it                                   |
|---------|-------------------------|----------------------------------|------------------------------------------------|
| Option  | `?T`                     | Represents a value that may be missing. | When an operation might return **no result** . |
| Result  | `Result.Result<T, E>`  where `T` is the success type and `E` is the error type.  | Represents success or failure.   | When an operation can either **succeed or fail**. |

## Options

Options provide a structured way to represent values that may or may not be present. Instead of using `null` directly, the option type enforces explicit handling, making programs safer and reducing unexpected failures.

An option is defined using `?` followed by the type of the value it can hold.

```motoko
var username: ?Text = null;
```

`username` is an optional [`Text`](/docs/motoko/base/Text) value that starts as `null` (no username set).

### Checking for presence

To determine if an option contains a value, `Option.isSome` returns `true` if it is not `null`.

```motoko no-repl
import Option "mo:base/Option";

let value: ?Nat = ?5;
if (Option.isSome(value)) {
    Debug.print("Value is present.");
}
```

By leveraging the `Option` module, handling optional values becomes more concise and expressive, reducing the need for explicit [`switch`](/docs/motoko/fundamentals/control-flow/switch) statements.

### Providing default values

Instead of manually handling `null` cases with pattern matching, `Option.get` allows for cleaner fallback logic:

```motoko no-repl
import Option "mo:base/Option";

let username: ?Text = null;
let displayName = Option.get(username, "Guest"); // "Guest" if username is null
```

This approach ensures that missing values are safely replaced with a default.

### Using options for error handling

Options can be used to catch expected failures instead of calling [`trap`](/docs/motoko/fundamentals/basic-syntax/traps), making a function return `null` when it encounters an invalid input.

```motoko no-repl
func safeDivide(a: Int, b: Int): ?Int {
    if (b == 0) null else ?(a / b);
}

let result1 = safeDivide(10, 2); // ?5
let result2 = safeDivide(10, 0); // null
```

This prevents division errors from interrupting program execution.

### Applying transformations to options

The `Option.map` function applies a transformation only if the value is present.

```motoko no-repl
import Option "mo:base/Option";

  let number: ?Nat = ?10;
  let doubled = Option.map<Nat, Nat>(number, func (x : Nat) = x * 2); // ?20
```

If `number` is `null`, `map` ensures the result remains `null` instead of performing an invalid operation.

### Applying an optional function

Sometimes, both the function and value are optional. `Option.apply` calls a function only if both are present.

```motoko no-repl
import Option "mo:base/Option";

let maybeIncrement: ?(Nat -> Nat) = ?(func x: Nat = x + 1);
let maybeValue: ?Nat = ?10;

let result = Option.apply<Nat,Nat>(maybeValue, maybeIncrement); // ?11
```

If either `maybeFunction` or `maybeValue` is `null`, the result remains `null`.

This is useful when chaining optional operations that may return `null`.

### Combining multiple optional values

When working with multiple optional values, using `Option.chain` processes them safely without unnecessary `switch` statements.

```motoko no-repl
import Option "mo:base/Option";

let firstName: ?Text = ?"Motoko";
let lastName: ?Text = ?"Ghost";

func combineNames(f: Text, l: Text): Text {
    f # " " # l;
};

let fullName = Option.chain<Text, Text>(firstName, func (f: Text) {
    Option.map<Text, Text>(lastName, func (l: Text) { combineNames(f, l) });
});

// ?("Motoko Ghost")
```

If either `firstName` or `lastName` is `null`, the result remains `null`.

### Unwrapping an option

```motoko
actor App {
  var username: ?Text = ?Motoko;

  public func getUsername() : async Text {
      switch (username) {
          case (?name) "Username: " # name;
          case null "No username set";
      }
  }
}
await App.getUsername();
```

To unwrap an option (`?T`), both cases must be handled:

1. When the value is present.
2. When it is absent (`null`).

If the value exists (`?value`), it can be accessed directly as its inner type (`T`) within the `case (?value)` branch. If the value is `null`, an alternative action or default value must be provided in the `case null` branch to ensure safe execution. This prevents runtime errors and ensures that optional values are handled explicitly.

### References

- [Option](https://internetcomputer.org/docs/current/motoko/main/base/option)


## Results

The `Result` type can hold:

- `#ok(value)`: Success with a value.
- `#err(errorMessage)`: Failure with an error message.

```motoko no-repl
import Result "mo:base/Result";
import Nat "mo:base/Nat";

func divide(a: Nat, b: Nat): Result.Result<Nat, Text> {
    if (b == 0) {
        return #err("Cannot divide by zero");
    };
    return #ok(a / b);
};
```

<img src="https://github.com/user-attachments/assets/844ca364-4d71-42b3-aaec-4a6c3509ee2e" alt="Logo" width="150" height="150" />