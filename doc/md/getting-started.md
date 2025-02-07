---
sidebar_position: 2
---

# Fundamentals

This section introduces the core concepts of Motoko, covering essential syntax, data types, and expressions. Before you begin writing canisters in Motoko, you should be familiar with a few of the basic concepts and terms specific to Motoko.

To learn through practical examples, visit the **[Motoko tutorial](https://internetcomputer.org/docs/current/motoko/tutorial/)**.  

## Basic syntax

### Defining an actor

In Motoko, an actor is a unit of computation that encapsulates state and behavior. Unlike traditional functions or objects in other programming languages, actors operate independently and interact with each other through asynchronous messaging.

A Motoko program typically starts by defining an actor. The example below declares an actor named `Main` with a `hello` function that returns a `"Hello, world!"` message:

```motoko
actor Main {
  public query func hello() : async Text {
    "Hello, world!"
  };
};

await Main.hello();
```

A Motoko actor always presents its interface as a suite of named functions with defined argument types and return types. The Motoko compiler and IC SDK generates this interface in **Candid**, a language-neutral format that enables communication between different systems.

Since actors in Motoko communicate asynchronously, `await` ensures the result is retrieved once the function completes.

### Imports  

Package imports should be at the top of the source file. You can import from:

Standard modules provided by the base library.  

  ```motoko
  import Text "mo:base/Text";
  ```  

Third-party packages installed via the Mops package manager.  

  ```motoko
  import Package "mo:packagename";
  ```  

Files within your own project.  

  ```motoko
  import Utils "utils";
  ```  

You can also import specific functions from a module:  

```motoko
import { compare } "mo:base/Nat";
```

### Printing values

Motoko uses `Debug.print` to output text to the terminal. It takes a `Text` value and returns `()`, meaning it has no meaningful return value.  

```motoko
import Debug "mo:base/Debug";
Debug.print("Hello, world!");
```

For debugging, `debug_show` converts most Motoko values into `Text`:  

```motoko
import Debug "mo:base/Debug";
Debug.print(debug_show(42)); // "42"
```

Since printing modifies output, it is considered an **impure function**, unlike pure functions that return values without side effects.

### Integers

Int: Represents all integers, both positive and negative (e.g., -2, -1, 0, 1, 2). By default, `Int` is unbounded, meaning it can grow as large as needed without overflow.

For scenarios requiring fixed-size integers, Motoko offers bounded variants with specific bit-widths (8, 16, 32, 64). These types can overflow if their limits are exceeded, resulting in a runtime error.

```motoko
let a: Int = -42;
let c: Int32 = 2147483647; // max bounded 32-bit integer
```

### Floats

Floating-point numbers in Motoko are represented using the `Float` type, which corresponds to a 64-bit double-precision floating-point number.

```motoko
let pi: Float = 3.14159;
let exp: Float = 2.71828;
```

### Characters

The `Char` type in Motoko represents a single unicode character (`'`) delimited.

```motoko
let letter: Char = 'A';
let symbol: Char = '☃';
```

### Text

Sequences of characters are handled using the `Text` type, which represents immutable strings of unicode characters (`"`) delimited.

```motoko
let greeting: Text = "Hello, world!";
```

### Literals

Literals are fixed values written directly in the code.

- Integer literals: `42`, `0x2A` (hexadecimal)
- Float literals: `3.14`, `2.5e3`
- Character literals: `'A'`, `'☃'`
- Text literals: `"Hello"`

### Whitespace

Whitespace characters (spaces, tabs, newlines) are generally ignored in Motoko but are essential for separating tokens like keywords and identifiers. Proper use of whitespace enhances code readability.

### Comments  

Motoko supports single-line, multi-line, and nested comments:

Use `//` for comments that extend to the end of a line.

  ```motoko
  // This is a single-line comment
  ```

Use `/* ... */` for block comments spanning multiple lines.

  ```motoko
  /* This is a
     multi-line comment */
  ```

Multi-line comments can be nested within each other.

  ```motoko
  /* Outer comment
     /* Nested comment */
     End of outer comment */
  ```

Use `///` for function or module documentation.

  ```motoko
  /// Returns the sum of two integers.
  func add(a: Int, b: Int): Int {
    a + b
  }
  ```

### Identifiers

Identifiers are names used for variables, functions, and other entities. They must start with a letter and can contain letters, digits, and underscores.

```motoko
let number = 10;
```

### Keywords

Motoko reserves certain words for its syntax and they cannot be used as identifiers. These include:

```motoko
actor      and        assert     async      async*     await      await*
break      case       catch      class      composite  continue   debug
debug_show do         else       false      flexible   finally    for
from_candid func      if         ignore     import     in         module
not        null       persistent object     or         label      let
loop       private    public     query      return     shared     stable
switch     system     throw      to_candid  true       transient  try
type       var        while      with
```

### Traps

A trap is a runtime error that causes execution to abort immediately. Common causes include division by zero, out-of-bounds array access and pattern match failure

If a trap occurs inside an actor message, only that message fails—other messages continue execution.  

To trigger a trap manually, use `Debug.trap`:  

```motoko
import Debug "mo:base/Debug";

Debug.trap("oops!");
```

### Assertions

An assertion checks a condition at runtime and traps if it fails.

```motoko
let n = 10;
assert n % 2 == 1; // Traps
```

```motoko
let n = 10;
assert n % 2 == 0; // Succeeds
```

Assertions help catch logic errors early but should not be used for regular error handling.

## Expressions

TBC

## Operators

Motoko provides various operators for working with numbers, text, and boolean values. They can be categorized as follows:

| **Category**   | **Description**                          | **Examples**  |
|---------------|----------------------------------|----------------------|
| Arithmetic | Math operations on numbers      | `+`, `-`, `*`, `/`, `%`, `**` |
| Bitwise    | Operations on individual bits   | `&`, `^`, `<<`, `>>`, `<<>`, `<>>` |
| Text       | Text concatenation              | `#` |
| Logical | Logical/Boolean operations       | `!`, `&`, |
| Ordered | Compare values                  | `==`, `!=`, `<`, `>` |

### Unary operators

| Operator | Description |
|----------|------------|
| `-`      | Numeric negation |
| `+`      | Numeric identity |
| `^`      | Bitwise negation |

### Relational operators

Relational operators compare two values and return `true` or `false`:  

| Operator | Description | Examples|
|----------|------------|-----------|
| `==`     | Equals | `a == b` |
| `!=`     | Not equals | `a != b`|
| `<`      | Less than | `a < b` |
| `>`      | Greater than | `a > b` |
| `<=`     | Less than or equal | `a <= b` |
| `>=`     | Greater than or equal | `a >= b`|

### Numeric binary operators

Binary operators combine two numbers to produce a result.

| Operator | Description |  Examples|
|----------|------------|------------|
| `+`      | Addition | `a + b` |
| `-`      | Subtraction | `a - b` |
| `*`      | Multiplication | `a * b` |
| `/`      | Division (integer division) | `a / b` |
| `%`      | Modulus (remainder) | `a % b` |
| `**`     | Exponentiation | `a ** b` |

⚠ **Note:**  Division (`/`) on integers **truncates** decimals. For floating-point division, use `Float.fromInt()`.  

```motoko
let result = Float.fromInt(10 / 3);
```

### Bitwise operators

Bitwise operators manipulate numbers **at the binary level**.  

| Operator | Description |Examples |
|----------|------------|----------|
| `&`      | Bitwise AND |`a & b` |
|  `|`    | Bitwise OR | `a | b` |
| `^`      | Bitwise XOR | `a ^ b` |
| `<<`     | Shift left | `a << b` |
| `>>`     | Shift right (must be proceeded by whitespace) |`a >> b` |
| `<<>`    | Rotate left (circular shift) | `a <<> b` |
| `<>`     | Rotate right (circular shift)| `a <> b` |

### Wrapping operators

Bounded integers **trap** on overflow, but **wrapping versions** allow overflow behavior:

| Operator | Description | Examples |
|----------|------------|------------|
| `+%`     | Addition with wrap-around | `a +% b` |
| `-%`     | Subtraction with wrap-around | `a -% b` |
| `*%`     | Multiplication with wrap-around | `a *% b` |
| `**%`    | Exponentiation with wrap-around | `a **% b` |

### Text operators

| Operator | Description | Example |
|----------|------------|------------|
| `#`      | Concatenates two `Text` values | `a # b` |

### Assignment operators

Assignment operators modify variables in place. Only mutable variables `var` can be reassigned.  

| Operator | Description |Examples|
|----------|------------|---------|
| `:=`     | Assign a value | `a := b` |
| `+=`     | Add and assign | `a += b` |
| `-=`     | Subtract and assign | `a -= b` |
| `*=`     | Multiply and assign | `a *= b` |
| `/=`     | Divide and assign | `a /= b` |
| `#=`     | Concatenate and assign (for `Text`) | `a #= b` |

### Operator precedence

Operators follow precedence rules, meaning some operators are evaluated before others.

1. Unary operators (`-`, `!`, `^`)  
2. Exponentiation (`**`, `**%`)  
3. Multiplication & Division (`*`, `/`, `%`, `*%`)  
4. Addition & Subtraction (`+`, `-`, `+%`, `-%`)  
5. Bitwise operators (`&`, `|`, `^`)  
6. Comparison operators (`==`, `!=`, `<`, `>`, `<=`, `>=`)  
7. Assignment operators (`:=`, `+=`, `-=`, etc.)  

For example:  

```motoko
let result = 10 + 5 * 2; // result = 20
```

Use parentheses to enforce a different order:

```motoko
let result = (10 + 5) * 2; // result = 30
```

### Pipes

Pipes (`|>`) chain function calls in a readable way. Instead of nesting function calls, pipes pass the result of one expression as an argument to the next function.

```motoko
func double(n: Int): Int { n * 2 };
func increment(n: Int): Int { n + 1 };

let result = 5 |> double(_) |> increment(_); // (5 * 2) + 1 = 11
```

<img src="https://github.com/user-attachments/assets/844ca364-4d71-42b3-aaec-4a6c3509ee2e" alt="Logo" width="150" height="150" />
