---
sidebar_position: 1
---

# Types of control flow

| Construct | Description |
|--------------|---------------|
| `return` | Exits a function and returns a value. |
| `if` | Executes a block if the condition is `true`. |
| `if/else` | Executes different blocks based on a condition. |
| `switch` | [Pattern matching](https://internetcomputer.org/docs/motoko/fundamentals/pattern-matching) for variants, options, results, etc. |
| `let-else` | Destructures a pattern and handle the failure case inline. |
| `option block` | Evaluates an expression and wraps the result in an option type, allowing scoped handling of `null` values. |
| `label/break` | Allows exiting loops early. |
| `while` | Runs while a condition is `true`. |
| `for` | Iterates over elements in a collection, terminating when no elements remain. |

## `while`

A `while` loop repeatedly executes a block of code as long as a specified condition evaluates to `true`.

```motoko no-repl
import Debug "mo:base/Debug";
import Nat "mo:base/Nat";

var i = 0;
while (i < 5) {
  Debug.print(Nat.toText(i));
  i += 1;
}
```

## `for`

A `for` loop iterates over elements in a collection or range, executing a block of code for each element.

``` motoko no-repl
import Debug "mo:base/Debug";
import Nat "mo:base/Nat";

let numbers = [1, 2, 3, 4, 5];
for (num in numbers.vals()) {
    Debug.print(Nat.toText(num));
}
```

## `return`

A `return` statement immediately exits a function and provides a result. Unlike `break` or `continue`, `return` stops execution entirely and sends a value back to the caller.

```motoko
func checkSign(n : Int) : Text {
  if (n > 0) {
    return "Positive";
  } else {
    return "Negative or zero";
  };
};
```

## `switch`

A `switch` expression matches a value against multiple cases and executes the block of code associated with the first matching case.

```motoko no-repl
import Nat "mo:base/Nat";

type HttpRequestStatus = {
  #ok: Nat;
  #err: Nat;
};

func checkStatus(r : HttpRequestStatus) : Text {
  switch (r) {
    case (#ok(successCode)) { "Success: " # Nat.toText(successCode) };
    case (#err(errorCode)) { "Failure: " # Nat.toText(errorCode) };
  };
};
```

## `let-else`

The `let-else` construct allows conditional binding by attempting to bind a value to a variable and immediately handling the case when the binding fails. It is useful when working with `Result<T,E>` and optional values (`?T`), enabling concise error handling or early exits when the value is `null`.

```motoko no-repl
import Nat "mo:base/Nat";

type HttpRequestStatus = {
  #ok: Nat;
  #err: Nat;
};

func checkStatus(r : HttpRequestStatus) : Text {
  let #ok(status) = r else return "The request failed!";
  Nat.toText(status)
};
```

:::note
Unlike a `switch`, `let-else` discards any additional error information from non-matching cases, making it less suitable when detailed error handling is needed. The `#err(e)` case is dropped entirely — `e` cannot be inspected or logged.
:::

## Option block

These blocks represented as `do ? {...}` allow safe unwrapping of optional values using the postfix operator `!`, which short-circuits and returns `null` if any value is `null`, simplifying code that handles multiple options.

```motoko no-repl
let x : ?Nat = foo(); // x might be null or have a Nat value
let y = ?Nat = foo(); // y might also be null or have a Nat value
let z = do ? { x! + 3 / y! }; // If x or y is null, z is also null; otherwise, z = x + 3 / y
```

Instead of having to switch `x` and `y` in a verbose manner the use of the postfix operator `!` makes it simple to unwrap.

## `label`

A `label` assigns a name to a block of code that executes like any other block, but its result can be accessed directly. Labels provide more control over execution, allowing clear exit points and helping to structure complex logic effectively.

When a labeled block runs, it evaluates its contents and returns a result. If no result is explicitly provided, it defaults to an empty value. Labels don’t change how the block executes but allow referencing and controlling its flow. Each labeled block must define an exit point.

```motoko no-repl
func labelControlFlow() : Int {
  let result = label processNumbers : Int {
    let numbers : [Int] = [3, -1, 0, 5, -2, 7];
    var sum : Int = 0;

    for (number in numbers.values()) {
      sum += number;
    };
    sum; // The final result of the block
  };
  return result;
}
```

### `break` within a labeled block

A `break` statement immediately exits a labeled block and returns a specified value. However, `break` must always refer to a label identifier; it cannot be used without one.

```motoko no-repl
func breakControlFlow() : Int {
  let result = label processNumbers: Int {
    let numbers : [Int] = [3, -1, 0, 5, -2, 7];
    var sum : Int = 0;

    for (num in numbers.values()) {
      if (num < 0) {
        break processNumbers sum; // Exit early with current sum
      };
      sum += num;
    };
    sum // This is returned if no break occurs
  };
  return result;
}
```

## Program flows

In Motoko, code executes sequentially, running one statement after another. However, certain constructs can alter this flow, such as exiting a block early, skipping iterations in a loop, returning a value from a function, or invoking another function.

### `continue`

A `continue` statement skips the remainder of the current iteration in a loop and immediately proceeds to the next iteration. Like `break`, `continue` must reference a label and only works within a labeled loop.

```motoko no-repl
func continueControlFlow() : Int {

  // Labeled block
  label processNumbers : Int {
    let numbers : [Int] = [3, -1, 0, 5, -2, 7];
    var sum : Int = 0;

    // Labeled loop
    label processing for (num in numbers.vals()) {
      if (num < 0) {
        continue processing; // Skip negative numbers
      };
      sum += num;
    };
    sum
  };
}
```

### Function calls

A function call executes a function by passing arguments and receiving a result. In Motoko, function calls can be synchronous (executing immediately within the same [canister](https://internetcomputer.org/docs/building-apps/essentials/canisters)) or [asynchronous](https://internetcomputer.org/docs/motoko/fundamentals/actors-async#async--await) (message passing between canisters). Asynchronous calls use `async`/`await` and are essential for inter-canister communication.

```motoko no-repl
import Nat "mo:base/Nat";

persistent actor {

  func processNumbers(numbers : [Int]) : Int {
    var sum : Int = 0;
    for (num in numbers.values()) {
      if (num < 0) {
        return sum;
      };
      sum += num;
    };
    return sum;
  };

  shared func functionCallControlFlow() : async Int {
    let numbers : [Int] = [3, 1, 5, -1, -2, 7];
    return processNumbers(numbers); // Function call
  };

}
```

Execution begins in `functionCallControlFlow()`, where the function `processNumbers()` is invoked, transferring control to its logic. Inside `processNumbers()`, the numbers are processed one by one. If a negative number is encountered, a `return` statement immediately halts the function and returns the current sum. Control then flows back to `functionCallControlFlow()`, which receives the result and returns it.

Function calls temporarily interrupt the normal sequential flow by shifting execution to a separate block of logic. Once the called function completes, control resumes at the point where the call was made.

<img src="https://cdn-assets-eu.frontify.com/s3/frontify-enterprise-files-eu/eyJwYXRoIjoiZGZpbml0eVwvYWNjb3VudHNcLzAxXC80MDAwMzA0XC9wcm9qZWN0c1wvNFwvYXNzZXRzXC8zOFwvMTc2XC9jZGYwZTJlOTEyNDFlYzAzZTQ1YTVhZTc4OGQ0ZDk0MS0xNjA1MjIyMzU4LnBuZyJ9:dfinity:9Q2_9PEsbPqdJNAQ08DAwqOenwIo7A8_tCN4PSSWkAM?width=2400" alt="Logo" width="150" height="150" />