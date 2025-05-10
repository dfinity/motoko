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

For example, define a `TodoError` type that the `markDone` function will use to signal errors:

```motoko no-repl
  public type TodoError = { #notFound; #alreadyDone : Time };
```

## Pattern matching with `Result`

In the case of a `Result`, you can also use pattern matching with the difference that you also get an informative value, not just `null`, in the `#err` case:

```motoko no-repl
func greetResult(resultName : Result<Text, Text>) : Text {
  let #ok(name) = resultname else return #err("No Name")
};
assert(greetResult(#ok("Motoko")) == "Hello, Motoko!");
assert(greetResult(#err("404 Not Found")) == "No name");
```

## Resources
- [`Result`](/docs/motoko/base/Option)