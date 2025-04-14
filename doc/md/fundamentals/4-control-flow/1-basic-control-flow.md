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
| `option block` | Evaluates an expression and wraps the result in an option type, allowing scoped handling of `null` values. |
| `label/break` | Allows exiting loops early. |
| `while` | Runs while a condition is `true`. |
| `for` | Iterates over elements in a collection, terminating when no elements remain. |
| `let-else` | Conditionally binds values. |

A return statement immediately exits a function and provides a result. Unlike `break` or `continue`, `return` stops execution entirely and returns a value to the caller.

```motoko no-repl
func add(a: Nat, b: Nat): Nat {
    return a + b;
}
```

Functions implicitly return the last evaluated expression if `return` is not used.

```motoko no-repl
func multiply(a: Nat, b: Nat): Nat {
    a * b  // Implicit return
}
```

## `if`

Conditional execution is controlled using `if`.

```motoko no-repl
func checkEven(n: Nat): Bool {
    if (n % 2 == 0) {
        return true;
    };
    return false;
}
```

## `if/else`

Executes one block if the condition is `true`, otherwise executes another.

```motoko no-repl
func checkSign(n: Int): Text {
    if (n > 0) {
        return "Positive";
    } else {
        return "Negative or Zero";
    };
}
```

## `switch`

A `switch` matches a value against multiple cases and executes the corresponding block of code.

```motoko no-repl
type HttpRequestStatus = {
    #ok: Nat;
    #err: Nat;
};

func checkStatus(r: HttpRequestStatus): Text {
    switch (r) {
        case (#ok(successCode)) { "Success: " # Nat.toText(value) } //eg. 200
        case (#err(errorCode)) { "Failure: " # Nat.toText(errorCode) } //eg. 404
    }
}
```

## `do ?`

The `do ?` syntax attempts to unwrap an optional value inside a block; if any expression inside evaluates to `null`, the entire block returns `null`.

```motoko no-repl
func getUserAge(user: ?Nat): ?Nat {
    do ? {
        let age = user!;  // If user is null, exit with null
        if (age < 18) null!; // null break to exit early if age is too low
        age
    }
  }
```
<!--- this contains a null break usage example, we should point a reader here for a search for null break--->

## `label`
A label assigns a name to a block of code that executes like any other, but its result can be accessed directly. Labels enable more control over execution, making it possible to define clear exit points and structure complex logic effectively.

When a labeled block runs, it evaluates its contents and produces a result. If no specific result is required, it defaults to an empty value. Labels do not alter how a block executes but provide a way to reference and control its flow. A labeled block is required to define an exit point.

```motoko no-repl
public func labelControlFlow() : async Int {
    label processNumbers : Int {
      let numbers : [Int] = [3, -1, 0, 5, -2, 7];
      var sum : Int = 0;

      for (number in numbers.vals()) {
        sum += number
      };
      sum // The final result of the block
    }
}
```

### `break` within a labeled block

A `break` statement stops execution inside a labeled block and returns a value immediately. However, a break must always reference an identifier. It cannot be used on its own.

```motoko no-repl
public func breakControlFlow() : async Int {
    label processNumbers: Int {
        let numbers : [Int] = [3, -1, 0, 5, -2, 7];
        var sum : Int = 0;

        for (num in numbers.vals()) {
            if (num < 0) {
                break processNumbers sum; // Exits early with current sum
            };
            sum += num;
        };
        sum // This is skipped if break is triggered
    };
}
```

## `while`

`while` repeatedly executes a block of code as long as a specified condition remains `true`.

```motoko no-repl
var i = 0;
while (i < 5) {
    Debug.print(Nat.toText(i));
    i += 1;
}
```

## `for`

A `for` loop iterates over elements in a collection or range, executing a block of code for each element.

``` motoko no-repl
let numbers = [1, 2, 3, 4, 5];
for (num in numbers.vals()) {
    Debug.print(Nat.toText(num));
}
```

## `let-else`

`let-else` allows conditional binding.

```motoko no-repl
let age = 18;
let authorized : Bool = if (age > 18) {true} else {false};
```


## Program flows

In Motoko, code follows sequential execution, running one statement after another. However, certain constructs allow altering this flow, including exiting a block early, skipping part of a loop, returning a value from a function or calling a function.


### `continue`

A `continue` statement skips the rest of the current iteration in a loop and moves directly to the next one. Like `break`, `continue` must reference a label. It works only within a labeled loop, ensuring controlled iteration.

```motoko no-repl
public func continueControlFlow() : async Int {

    // Labeled block
    label processNumbers: Int {
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

A function call executes a function by passing arguments and receiving a result. In Motoko, function calls may involve synchronous execution within a canister or [asynchronous](https://internetcomputer.org/docs/motoko/fundamentals/actors-async#async--await) messaging between [canisters](https://internetcomputer.org/docs/building-apps/essentials/canisters).

```motoko no-repl
public func processNumbers(numbers: [Int]) : Int {
    var sum : Int = 0;
        for (num in numbers.vals()) {
            if (num < 0) {
                return sum;
            };
            sum += num;
        };
    return sum;
}

public func functionCallControlFlow() : async Int {
    let numbers : [Int] = [3, 1, 5, -1, -2, 7];
    return processNumbers(numbers); // Function call
}
```

Execution begins in `functionCallExample()`, where the function `processNumbers()` is invoked, shifting control to its execution. Within `processNumbers()`, numbers are processed sequentially. If a negative number is encountered, `return` halts execution immediately and returns the current sum. Control then transfers back to `functionCallExample()`, which receives and returns the final result. Function calls disrupt the normal sequential flow by directing execution to a separate block of logic.

<img src="https://cdn-assets-eu.frontify.com/s3/frontify-enterprise-files-eu/eyJwYXRoIjoiZGZpbml0eVwvYWNjb3VudHNcLzAxXC80MDAwMzA0XC9wcm9qZWN0c1wvNFwvYXNzZXRzXC8zOFwvMTc2XC9jZGYwZTJlOTEyNDFlYzAzZTQ1YTVhZTc4OGQ0ZDk0MS0xNjA1MjIyMzU4LnBuZyJ9:dfinity:9Q2_9PEsbPqdJNAQ08DAwqOenwIo7A8_tCN4PSSWkAM?width=2400" alt="Logo" width="150" height="150" />