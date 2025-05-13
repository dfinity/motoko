---
sidebar_position: 11
---

# Results

| Type    | Syntax                  | Purpose                          | When to use it                                   |
|---------|-------------------------|----------------------------------|------------------------------------------------|
| Result  | `Result.Result<T, E>`  where `T` is the success type and `E` is the error type.  | Represents success or failure.   | When an operation can either **succeed or fail**. |

While options are a built-in type, the `Result` is defined as a variant type:

`type Result<Ok, Err> = { #ok : Ok; #err : Err }`

Because of the second type parameter, `Err`, the `Result` type lets you select the type used to describe errors.

Here’s a simple function that validates a username. If it's non-empty, we return a greeting of type `Text`. If not, return an error of type `Text`.

```motoko
import Result "mo:base/Result";

func greet(name : Text) : Result.Result<Text, Text> {
  if (name.size() == 0) #err "Name must be present" else #ok ("Hello" # name);
};
```

## Pattern matching with `Result`

When a Motoko value has type `Result<T, E>`, it is either a success, written `#ok value`, or a failure, written `#err error`. The fundamental way to access the contents is to use a `switch` expression that explicitly handles both cases.

### `switch`

```motoko
import Result "mo:base/Result";

func greet(name : Text) : Result.Result<Text, Text> {
  if (name.size() == 0) #err "Name must be present" else #ok ("Hello " # name);
};

let result = greet("motoko");
switch (result){
  case (#ok message) {
    debug_show(message) // Hello Motoko
  };
  case (#err error){
    debug_show(error)
  };
}
```

The verbosity of `switch` expressions can make code harder to read, so Motoko also offers additional ways to handle `Result`.

### `let else`

Values can be extracted from Result using the `let ... else` pattern. This can be preferable to a `switch` expression when only the success `case` is needed, and the else branch can cleanly handle or exit on failure. It allows concise early returns or alternative flows when the result is `#err`.

```motoko
import Result "mo:base/Result";

func greet(name : Text) : Result.Result<Text, Text> {
  if (name.size() == 0) #err "Name must be present" else #ok ("Hello " # name);
};

func safeGreet(name : Text) : Text {
  let #ok greeting = greet(name) else return "Default greeting: Hi!";
  greeting
};

safeGreet("");      // returns "Default greeting: Hi!"
```

:::info
A default alternative flow is provided when an error is encountered. This ensures that control flow remains uninterrupted—there is no need to handle both cases explicitly, and the function continues  with a fallback result in the event of failure.
:::

## Resources

- [`Result`](/docs/motoko/base/Result)
