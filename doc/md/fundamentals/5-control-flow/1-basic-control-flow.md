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
| `let-else` | Destructure a pattern and handle the failure case inline. |
| `option block` | Evaluates an expression and wraps the result in an option type, allowing scoped handling of `null` values. |
| `label/break` | Allows exiting loops early. |
| `while` | Runs while a condition is `true`. |
| `for` | Iterates over elements in a collection, terminating when no elements remain. |

## `return`

A `return` statement immediately exits a function or `async` block with a result. Unlike `break` or `continue`, which jump to a labeled point within the same function, `return` does not target a label. Instead, it exits the current function entirely, either returning control to the caller or, in asynchronous contexts, completing a future and resuming the caller that's awaiting the result.

```motoko
Consider this function that computes the product of an array of integers.

``` motoko
func product(numbers : [Int]) : Int {
  var prod : Int = 1;
  for (number in numbers.vals()) {
    prod *= number;
  };
  prod; // The implicit result of the block and function
}
```

This function doesn't require an explicit `return`. It just returns the result of its body, `prod`.

However, `prod` will remain `0` once it becomes `0` so you can save some work by returning from the function early, exiting both the loop and the function with result `0`.

``` motoko
func product(numbers : [Int]) : Int {
  var prod : Int = 1;
  for (number in numbers.vals()) {
    prod *= number;
    if (prod == 0) return 0; // an early return can save work
  };
  prod; // The implicit result of the block and function
}
```

This also works with asynchronous functions that produce futures:

``` motoko
func asyncProduct(numbers : [Int]) : async Int {
  var prod : Int = 1;
  for (number in numbers.vals()) {
    prod *= number;
    if (prod == 0) return 0; // an early return completes the future
  };
  prod; // The implicit result of the block and function
}
```

If the expected return type is `()` then you can just write `return` instead of `return ()`.

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
    case (#ok successCode) { "Success: " # Nat.toText(successCode) };
    case (#err errorCode ) { "Failure: " # Nat.toText(errorCode) };
  };
};
```

## `let-else`

The `let-else` construct allows conditional binding of the variables in a pattern, by attempting to match a value to the pattern. The `else` clause handles the case when the pattern is not a match. It is useful when working with `Result<T,E>` and optional values (`?T`), enabling concise error handling or early exits when the value is `null`.

Since the code following the `let` cannot execute without its matching bindings, the `else` clause must have type `None`, typically by diverting control using `return`, `throw`, `break` or `continue`.

```motoko no-repl
import Nat "mo:base/Nat";

type HttpRequestStatus = {
  #ok: Nat;
  #err: Nat;
};

func checkStatus(r : HttpRequestStatus) : Text {
  let #ok status = r else return "The request failed!";
  Nat.toText(status)
};
```

:::note
Unlike a `switch`, `let-else` discards any additional error information from non-matching cases, making it less suitable when detailed error handling is needed. The `(#err e)` case is dropped entirely; `e` cannot be inspected or logged.
:::

## Option block

These blocks represented as `do ? {...}` allow safe unwrapping of optional values using the postfix operator `!`, which short-circuits and exits the block with `null` if any value is `null`, simplifying code that handles multiple options. The result of the inner block, if any, is returned in an option.

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
};

let expr : Exp = #If(
  #Div(#Lit 10, #Lit 2),   // 10 / 2 = 5 (non-zero, so evaluate e3)
  #Lit 0,                  // e2 (ignored because e1 ≠ 0)
  #Div(#Lit 6, #Lit 3)     // e3 → 6 / 3 = 2
);

eval(expr);
```

Instead of having to switch on the options `n` and `m` in a verbose manner the use of the postfix operator `!` makes it easy to unwrap their values but exit the block with `null` when either is `null`.

## `label`

A `label` assigns a name with an optional type to a block of code that executes like any other block, but its result can be produced early using a `break` to the label. The type on the label should indicate the type of the block and defaults to `()` when omitted.


When a labeled block runs, it evaluates the block to produce a result. Labels don’t change how the block executes but enable early exits from the block using a `break` to that label. If the type is not `()` those breaks must have an argument, to use as the result of the labelled expression. Just as `return` exits a function early with a result, `break` exits its label early with a result.


```motoko no-repl
func product(numbers : [Int]) : Int {
  var prod : Int = 1;
  label l for (number in numbers.vals()) {
    prod *= number;
    if (prod == 0) break l;
  };
  prod; // The implicit result of the block and function
}

If the block produces a non-`()` result, the `break` can include a value.


```motoko no-repl
func product(numbers : [Int]) : Int {
  label ret : Int {
    var prod : Int = 1;
    for (number in numbers.vals()) {
      prod *= number;
      if (prod == 0) break ret 0;
    };
    prod
 }
}
```

Labels provide more control over execution, allowing clear exit points and helping to structure complex logic effectively.

### `break` within a labeled block

A `break` expression immediately exits a labeled expression and returns a specified value. However, `break` must always refer to a label identifier; it cannot be used without one.

```motoko no-repl
  func breakControlFlow() : Int {
    let result = label processNumbers : Int {
      let numbers : [Int] = [3, 1, 5, 0, 2, 7];
      var prod : Int = 1;

      label loop1 for (num in numbers.vals()) {
        if (num < 0) {
          break processNumbers 0; // Exit early if prod becomes 0
        };
        prod *= num;
        if (prod == 0) return 0; // an early return can save work

      };
      prod // Returned if no early exit occurred
    };
    return result;
  };
```

As with `return`, you can omit the value argument to a `break` when the type of the label is `()`, as in this restructured code:

```motoko no-repl
  func breakControlFlow() : Int {
    let result = label processNumbers : Int {
      let numbers : [Int] = [3, 1, 5, 0, 2, 7];
      var prod : Int = 1;

      label loop1 for (num in numbers.vals()) {
        if (num < 0) {
          break processNumbers 0; // Exit early if prod becomes 0
        };
        prod *= num;
        if (prod == 0) return 0; // an early return can save work

      };
      prod // Ensure a value is returned if no early break occurs
    };
    result
  };
```

## `loop`

A `loop` expression repeatedly executes a block of code (forever).

```motoko no-repl
import Debug "mo:base/Debug";
import Nat "mo:base/Nat";

var i = 0;
loop {
  Debug.print(Nat.toText(i));
  i += 1;
}
```

## `loop-while`

A `loop-while` expression repeatedly executes a block of code (at least once) until the while condition evaluates to `false`.

```motoko no-repl
import Debug "mo:base/Debug";
import Nat "mo:base/Nat";

var i = 0;
loop {
  Debug.print(Nat.toText(i));
  i += 1;
} while (i < 5)
```


## `while`

A `while` loop repeatedly executes a block of code as long as a specified condition evaluates to `true`.
If the condition is initially `false`, the block is never executed.

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

A `for` loop iterates over the elements of an iterator, and object of type `{ next: () -> ?T }`, executing a block of code for each element.

``` motoko no-repl
import Debug "mo:base/Debug";
import Nat "mo:base/Nat";

let numbers = [1, 2, 3, 4, 5];
for (num in numbers.vals()) {
    Debug.print(Nat.toText(num));
}
```

It will run forever if the iterator's `next` method never returns `null`.

## `continue`

A `continue` expression skips the remainder of the current iteration in a loop and immediately proceeds to the next iteration. Like `break`, `continue` must reference a label and only works within a labeled `while`, `for` or `loop` or `loop-while` expression.

```motoko no-repl
  func breakControlFlow() : Int {
    let result = label processNumbers : Int {
      let numbers : [Int] = [3, 1, 5, 0, 2, 7];
      var prod : Int = 1;

      label loop1 for (num in numbers.vals()) {
        if (num < 1) {
          continue loop1;
        };
        prod *= num;
        if (prod == 0) return 0; // an early return can save work

      };
      prod //
    };
    result
  };
```
## loop exits

You can alway exit a labelled `loop`, `loop-while`, `while` or `for` loop or using `break` and any loop in a function using `return` or (in an asynchronous function) `throw`.

## Program flows

In Motoko, code executes sequentially, evaluating expressions and declarations in order. However, certain constructs can alter this flow, such as exiting a block early, skipping iterations in a loop, returning a value from a function, or invoking another function.


### Function calls

A function call executes a function by passing arguments and receiving a result. In Motoko, function calls can be synchronous (executing immediately within the same [canister](https://internetcomputer.org/docs/building-apps/essentials/canisters)) or [asynchronous](https://internetcomputer.org/docs/motoko/fundamentals/actors-async#async--await) (message passing between canisters). Asynchronous calls use `async`/`await` and are essential for inter-canister communication.

```motoko no-repl
import Nat "mo:base/Nat";

persistent actor {

  func processNumbers(numbers : [Int]) : Int {
    var prod : Int = 0;
    for (num in numbers.values()) {
      if (num < 0) {
        return prod;
      };
      prod += num;
      if (prod == 0) return 0; // an early return can save work

    };
    return prod;
  };

  shared func functionCallControlFlow() : async Int {
    let numbers : [Int] = [3, 1, 5, -1, -2, 7];
    return processNumbers(numbers); // Function call
  };

}
```

Execution begins in `functionCallControlFlow()`, where the function `processNumbers()` is invoked, transferring control to its logic. Inside `processNumbers()`, the numbers are processed one by one. If a negative number is encountered, a `return` statement immediately halts the function and returns the current sum. Control then flows back to `functionCallControlFlow()`, which receives the result and returns it.

Function calls temporarily interrupt the normal sequential flow by shifting execution to a separate block of logic. Once the called function completes, control resumes at the point where the call was made, continuing with its result.

