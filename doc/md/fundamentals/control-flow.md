---
sidebar_position: 14
---

# Control flow

| Construct | Description |
|--------------|---------------|
| `return` | Exits a function and returns a value. |
| `if` | Executes a block if the condition is `true`. |
| `if/else` | Executes different blocks based on a condition. |
| `switch` | Pattern matching for variants, options, results, etc. |
| `option block` | Evaluates an expression and wraps the result in an option type, allowing scoped handling of `null` values. |
| `label/break` | Allows exiting loops early. |
| `while` | Runs while a condition is `true`. |
| `for` | Iterates over elements in a collection, terminating when no elements remain. |
| `let-else` | Conditionally binds values. |

## `return`

A return statement immediately exits a function and provides a result.

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

Matches a value against multiple cases and executes the corresponding block of code.

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

Attempts to unwrap an optional value inside a block; if any expression inside evaluates to `null`, the entire block returns `null`.

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

## `label` and `break`

Loops can be named using `label` and exited early with `break`.

```motoko no-repl
label outer loop {
    var x = 0;
    while (x < 5) {
        x += 1;
        if (x == 3) {
            break outer;  // Exits the entire labeled loop
        };
    };
}
```

## `while`

Repeatedly executes a block of code as long as a specified condition remains `true`.

```motoko no-repl
var i = 0;
while (i < 5) {
    Debug.print(Nat.toText(i));
    i += 1;
}
```

## `for`

Iterates over elements in a collection or range, executing a block of code for each element.

``` motoko no-repl
let numbers = [1, 2, 3, 4, 5];
for (num in numbers.vals()) {
    Debug.print(Nat.toText(num));
}
```

## `let-else`

Let-else allows conditional binding.

```motoko no-repl
let age = 18;
let authorized : Bool = if (age > 18) {true} else {false};
```
