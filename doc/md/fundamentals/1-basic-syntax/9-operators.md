---
sidebar_position: 9
---

# Operators

Motoko provides various operators for working with numbers, text, and boolean values. They can be categorized as follows:

| **Category**   | **Description**                          | **Examples**  |
|---------------|----------------------------------|----------------------|
| Arithmetic | Math operations on numbers      | `+`, `-`, `*`, `/`, `%`, `**` |
| Bitwise    | Operations on individual bits   | `&`, `|`, ^`, `<<`, `>>`, `<<>`, `<>>` |
| Text       | Text concatenation              | `#` |
| Logical | Logical/boolean operations       | `not`, `and`, `or` |
| Ordered | Comparing values                  | `==`, `!=`, `<`, `>` |

:::info

Bitwise operators can only be used with bounded types.

:::

## Unary operators

| Operator | Description |
|----------|------------|
| `-`      | Numeric negation |
| `+`      | Numeric identity |
| `^`      | Bitwise negation |
| `do ?`   | Option block     |

## Relational operators

Relational operators compare two values and return `true` or `false`.

| Operator | Description | Example|
|----------|------------|-----------|
| `==`     | Equals | `a == b` |
| `!=`     | Not equals | `a != b`|
| `<`      | Less than | `a < b` |
| `>`      | Greater than | `a > b` |
| `<=`     | Less than or equal | `a <= b` |
| `>=`     | Greater than or equal | `a >= b`|

## Numeric binary operators

Binary operators combine two numbers to produce a result.

| Operator | Description |  Example|
|----------|------------|------------|
| `+`      | Addition | `a + b` |
| `-`      | Subtraction | `a - b` |
| `*`      | Multiplication | `a * b` |
| `/`      | Division (integer division) | `a / b` |
| `%`      | Modulus (remainder) | `a % b` |
| `**`     | Exponentiation | `a ** b` |

:::warning

Division (`/`) on integers **truncates** decimals. For floating-point division, use `Float.fromInt()`:

```motoko no-repl
let result = Float.fromInt(10) / Float.fromInt(3);
```

:::

## Bitwise operators

Bitwise operators manipulate numbers **at the binary level**.

| Operator | Description |Example |
|----------|------------|----------|
| `&`      | Bitwise AND |`a & b` |
|  `|`    | Bitwise OR | `a | b` |
| `^`      | Bitwise XOR | `a ^ b` |
| `<<`     | Shift left | `a << b` |
| `>>`     | Shift right (must be proceeded by whitespace) |`a >> b` |
| `<<>`    | Rotate left (circular shift) | `a <<> b` |
| `<>`     | Rotate right (circular shift)| `a <> b` |

:::caution

Bitwise operators can only be used with bounded types.

:::

## Wrapping operators

Bounded integers **trap** on overflow, but **wrapping versions** allow overflow behavior.

| Operator | Description | Example |
|----------|------------|------------|
| `+%`     | Addition with wrap-around | `a +% b` |
| `-%`     | Subtraction with wrap-around | `a -% b` |
| `*%`     | Multiplication with wrap-around | `a *% b` |
| `**%`    | Exponentiation with wrap-around | `a **% b` |

## Text operators

| Operator | Description | Example |
|----------|------------|------------|
| `#`      | Concatenates two [`Text`](https://internetcomputer.org/docs/motoko/base/Text) values | `a # b` |

## Assignment operators

Assignment operators modify variables in place. Both mutable variables declared with `var` and elements of mutable arrays can be assigned new values.

| Operator | Description |Examples|
|----------|------------|---------|
| `:=`     | Assign a value | `a := b` |
| `+=`     | Add and assign | `a += b` |
| `-=`     | Subtract and assign | `a -= b` |
| `*=`     | Multiply and assign | `a *= b` |
| `/=`     | Divide and assign | `a /= b` |
| `#=`     | Concatenate and assign (for [`Text`](https://internetcomputer.org/docs/motoko/base/Text)) | `a #= b` |

For example:  

```motoko
var done = false; done := true;

let a = [var 1, 2];
a[0] += a[1];
```

## Operator precedence

Operators follow precedence rules, meaning that, in the absence of explicit parentheses, some operators are evaluated before others.

1. Unary operators (`-`, `!`, `^`)
2. Exponentiation (`**`, `**%`)
3. Multiplication & division (`*`, `/`, `%`, `*%`)
4. Addition & subtraction (`+`, `-`, `+%`, `-%`)
5. Bitwise operators (`&`, `|`, `^`)
6. Comparison operators (`==`, `!=`, `<`, `>`, `<=`, `>=`)
7. Assignment operators (`:=`, `+=`, `-=`, etc.)

For example:

```motoko
let result = 10 + 5 * 2; // result = 20
```

Use parentheses to enforce a different order.

```motoko
let result = (10 + 5) * 2; // result = 30
```

## Pipes

Pipes (`|>`) chain together function calls in a readable way. Instead of nesting function calls, pipes pass the result of one expression as an argument to the next function. The value of the left side of the pipe is referenced on the right side using an underscore (`_`).

```motoko
func double(n : Int) : Int { n * 2 };
func increment(n : Int) : Int { n + 1 };

let result = 5 |> double(_) |> increment(_); // (5 * 2) + 1 = 11
```

## Option blocks (`do ?`)

Option blocks use the syntax `do ? <block>` to handle optional values of type `?T` without needing nested switch statements. It produces a value of type `?T`, when <block> has type `T` and, importantly, introduces the possibility of a break from <block>. 

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

```motoko no-repl
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
      case (#IfZero (e1, e2, e3)) {
        if (eval e1 ! == 0)  // Unwrap and check if zero
          eval e2 !  // Return result of e2 (or null if it's null)
        else
          eval e3 !  // Return result of e3 (or null if it's null)
      };
    };
  };
}
```

To guard against division by 0 without trapping, the eval function returns an option result, using null to indicate failure.

Each recursive call is checked for null using !, immediately exiting the outer do ? block, and then the function itself, when a result is null.


<img src="https://cdn-assets-eu.frontify.com/s3/frontify-enterprise-files-eu/eyJwYXRoIjoiZGZpbml0eVwvYWNjb3VudHNcLzAxXC80MDAwMzA0XC9wcm9qZWN0c1wvNFwvYXNzZXRzXC8zOFwvMTc2XC9jZGYwZTJlOTEyNDFlYzAzZTQ1YTVhZTc4OGQ0ZDk0MS0xNjA1MjIyMzU4LnBuZyJ9:dfinity:9Q2_9PEsbPqdJNAQ08DAwqOenwIo7A8_tCN4PSSWkAM?width=2400" alt="Logo" width="150" height="150" />