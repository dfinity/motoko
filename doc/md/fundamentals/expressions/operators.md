---
sidebar_position: 2
---

# Operators

Motoko provides various operators for working with numbers, text, and boolean values. They can be categorized as follows:

| **Category**   | **Description**                          | **Examples**  |
|---------------|----------------------------------|----------------------|
| Arithmetic | Math operations on numbers      | `+`, `-`, `*`, `/`, `%`, `**` |
| Bitwise    | Operations on individual bits   | `&`, `^`, `<<`, `>>`, `<<>`, `<>>` |
| Text       | Text concatenation              | `#` |
| Logical | Logical/Boolean operations       | `!`, `&`, |
| Ordered | Compare values                  | `==`, `!=`, `<`, `>` |

## Unary operators

| Operator | Description |
|----------|------------|
| `-`      | Numeric negation |
| `+`      | Numeric identity |
| `^`      | Bitwise negation |

## Relational operators

Relational operators compare two values and return `true` or `false`:  

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

⚠ **Note:**  Division (`/`) on integers **truncates** decimals. For floating-point division, use `Float.fromInt()`.  

```motoko
let result = Float.fromInt(10) / Float.fromInt(3);
```

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

## Wrapping operators

Bounded integers **trap** on overflow, but **wrapping versions** allow overflow behavior:

| Operator | Description | Example |
|----------|------------|------------|
| `+%`     | Addition with wrap-around | `a +% b` |
| `-%`     | Subtraction with wrap-around | `a -% b` |
| `*%`     | Multiplication with wrap-around | `a *% b` |
| `**%`    | Exponentiation with wrap-around | `a **% b` |

## Text operators

| Operator | Description | Example |
|----------|------------|------------|
| `#`      | Concatenates two `Text` values | `a # b` |

## Assignment operators

Assignment operators modify variables in place. Only mutable variables `var` can be reassigned.  

| Operator | Description |Examples|
|----------|------------|---------|
| `:=`     | Assign a value | `a := b` |
| `+=`     | Add and assign | `a += b` |
| `-=`     | Subtract and assign | `a -= b` |
| `*=`     | Multiply and assign | `a *= b` |
| `/=`     | Divide and assign | `a /= b` |
| `#=`     | Concatenate and assign (for `Text`) | `a #= b` |

## Operator precedence

Operators follow precedence rules, meaning some operators are evaluated before others.

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

Use parentheses to enforce a different order:

```motoko
let result = (10 + 5) * 2; // result = 30
```

## Pipes

Pipes (`|>`) chain function calls in a readable way. Instead of nesting function calls, pipes pass the result of one expression as an argument to the next function. The value of the left hand side of the pipe is referenced on the right-hand-side using an underscore `_`.

```motoko
func double(n: Int): Int { n * 2 };
func increment(n: Int): Int { n + 1 };

let result = 5 |> double(_) |> increment(_); // (5 * 2) + 1 = 11
```
