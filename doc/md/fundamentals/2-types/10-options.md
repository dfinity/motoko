---
sidebar_position: 10
---

# Options & results

| Type    | Syntax                  | Purpose                          | When to use it                                   |
|---------|-------------------------|----------------------------------|------------------------------------------------|
| Option  | `?T`                     | Represents a value that may be missing. | When an operation might return **no result**. |

## Options

Options provide a structured way to represent values that may or may not be present. Instead of using `null` directly, the option type enforces explicit handling, making programs safer and reducing unexpected failures.

An option is defined using `?` followed by the type of the value it can hold.

```motoko no-repl
var username : ?Text = null;
```

`username` is an optional [`Text`](https://internetcomputer.org/docs/motoko/base/Text) value that starts as `null` (no username set).

:::info Null semantics

`null` is only valid as a value of the trivial `Null` type or an optional type (`?T`). This makes it similar to `None` in languages like Rust, Scala, and `Nothing` in Haskell. In all these languages, the type system also enforces explicit handling of missing values.

:::

### Checking for presence

To determine if an option contains a value, `Option.isSome` returns `true` if it is not `null`.

```motoko no-repl
import Option "mo:base/Option";

let value : ?Nat = ?5;
if (Option.isSome(value)) {
    Debug.print("Value is present.");
}
```

By leveraging the `Option` module, handling optional values becomes more concise and expressive, reducing the need for explicit [`switch`](https://internetcomputer.org/docs/motoko/fundamentals/control-flow/switch) statements.

### Providing default values

Instead of manually handling `null` cases with [pattern matching](https://internetcomputer.org/docs/motoko/fundamentals/pattern-matching), `Option.get` allows for cleaner fallback logic to ensure that missing values are safely replaced with a default.

```motoko no-repl
import Option "mo:base/Option";

let username : ?Text = null;
let displayName = Option.get(username, "Guest"); // "Guest" if username is null
```

Then same functionality can be achieved using `let-else`

```motoko no-repl
let username : ?Text = null;
let ?displayName =  username else return "Guest" // "Guest" if username is null
```

### Using options for error handling

Options can be used to catch expected failures instead of calling a [`trap`](https://internetcomputer.org/docs/motoko/fundamentals/basic-syntax/traps), making a function return `null` when it encounters an invalid input.

```motoko no-repl
func safeDivide(a : Int, b : Int) : ?Int {
    if (b == 0) null else ?(a / b);
}

let result1 = safeDivide(10, 2); // ?5
let result2 = safeDivide(10, 0); // null
```

### Let / else

To safely extract values from options, use the `let ... else` pattern. This is often preferred and encoraged over nested `switch` expressions when a fallback is needed, as it improves readability and ensures fallbacks are handled.

For example, here’s a simple implementation of an option helper:

```motoko no-repl
func get<T>(option : ?T, defaultValue : T) : T {
  let ?value = option else return defaultValue;
  return value;
};

assert get(?"A", "B") == "A";
assert get(null, "B") == "B";
```

### Applying transformations to options

The `Option.map` function applies a transformation only if the value is present.

```motoko no-repl
import Option "mo:base/Option";

let number : ?Nat = ?10;
let doubled = Option.map<Nat, Nat>(number, func(x : Nat) = x * 2); // ?20
```

In this example, if `number` is `null`, `map` ensures the result remains `null` instead of performing an invalid operation.

### Applying an optional function

Sometimes, both the function and value are optional. `Option.apply` calls a function only if both are present. This is useful when chaining optional operations that may return `null`.

```motoko no-repl
import Option "mo:base/Option";

let maybeIncrement : ?(Nat -> Nat) = ?(func x: Nat = x + 1);
let maybeValue : ?Nat = ?10;

let result = Option.apply<Nat, Nat>(maybeValue, maybeIncrement); // ?11
```

If either `maybeFunction` or `maybeValue` is `null`, the result remains `null`.

### Combining multiple optional values

When working with multiple optional values, using `Option.chain` processes them safely without unnecessary `switch` statements.

```motoko no-repl
import Option "mo:base/Option";

let firstName : ?Text = ?"Motoko";
let lastName : ?Text = ?"Ghost";

func combineNames(f : Text, l : Text) : Text {
    f # " " # l;
};

let fullName = Option.chain<Text, Text>(firstName, func (first : Text) {
    Option.map<Text, Text>(lastName, func(last : Text) { combineNames(first, last) });
});

// ?("Motoko Ghost")
```

If either `firstName` or `lastName` is `null`, the result remains `null`.

### Unwrapping an option

```motoko
persistent actor App {
  public func getName(optionalName : ?Text) : async Text {
    let ?name = optionalName else return "Unknown";
    name
  }
};
await App.getName(null);
```

To unwrap an option (`?T`), both cases must be handled:

1. When the value is present.
2. When it is absent (`null`).

If the value exists (`?value`), it can be accessed directly as its inner type (`T`) within the `case (?value)` branch. If the value is `null`, an alternative action or default value must be provided in the `case null` branch to ensure safe execution. This prevents runtime errors and ensures that optional values are handled explicitly.

## Option blocks (`do ?`)

Option blocks use the syntax `do ? <block>` to handle optional values of type `?T` without needing nested switch statements. It produces a value of type `?T`, when `<block>` has type `T` and, importantly, introduces the possibility of a break from `<block>.`

Within a `do ? <block>`, the `null` break `<exp> !` tests whether the result of the expression, `<exp>` of unrelated option type, `?U`, is `null`.

If the result is `null`, control immediately exits the `do ? <block>` with value `null`. Otherwise, the result of `<exp>` must be an option value `?v`, and evaluation of `<exp> !` proceeds with its contents, `v` of type `U`.

The `do ? <block>` is similar to how the `?` operator works in languages like Rust.

```motoko no-repl
// Introduces an option block that returns a value of type ?T
let result: ?T = do ? {
  // The ! operator (null break) unwraps optional values inside the block
  someOptionalValue!  // unwraps the value or short-circuits
}
```

The following example defines a simple function that evaluates expressions built from natural numbers, division and a zero test, encoded as a variant type:

```motoko
type Exp = {#Lit : Nat; #Div : (Exp, Exp); #If : (Exp, Exp, Exp)};
func eval(e : Exp) : ? Nat {
  do ? {
    switch e {
      case (#Lit n) { n };
      case (#Div (e1, e2)) {
        let v1 = eval e1 !;  // If eval e1 returns null, exit with null
        let v2 = eval e2 !;  // If eval e2 returns null, exit with null
        if (v2 == 0)
          null !  // Explicitly exit with null for division by zero
        else v1 / v2
      };
      case (#If (e1, e2, e3)) {
        if (eval e1 ! == 0)  // Unwrap and check if zero
          eval e2 !  // Return result of e2 (or null if it's null)
        else
          eval e3 !  // Return result of e3 (or null if it's null)
      };
    };
  };
}
```

To guard against division by 0 without trapping, the eval function returns an option result, using `null` to indicate failure.

Each recursive call is checked for `null` using `!`, immediately exiting the outer `do ?` block, and then the function itself, when a result is `null`.

## Resources

- [`Option`](/docs/motoko/base/Option)


<img src="https://cdn-assets-eu.frontify.com/s3/frontify-enterprise-files-eu/eyJwYXRoIjoiZGZpbml0eVwvYWNjb3VudHNcLzAxXC80MDAwMzA0XC9wcm9qZWN0c1wvNFwvYXNzZXRzXC8zOFwvMTc2XC9jZGYwZTJlOTEyNDFlYzAzZTQ1YTVhZTc4OGQ0ZDk0MS0xNjA1MjIyMzU4LnBuZyJ9:dfinity:9Q2_9PEsbPqdJNAQ08DAwqOenwIo7A8_tCN4PSSWkAM?width=2400" alt="Logo" width="150" height="150" />