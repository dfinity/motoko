---
sidebar_position: 20
---

# Piping values into expressions



It can sometimes be hard to read deeply nested expressions involving several function applications.

``` motoko file=../examples/Unpiped.mo#L1-L8
```

This expression takes the range of numbers `0`..`10`, converts it to a list, filters the list for multiples of three, then returns a record containing the result.

To make such expressions more readable, you can use Motoko's pipe operator `<exp1> |> <exp2>`.
The operator evaluates the first argument `<exp1>`, and lets you refer to its value in `<exp2>` using the special placeholder expression `_`.

Using this, you can write the former expression as:

``` motoko file=../examples/Piped.mo#L1-L8
```

Now, the textual order of operations corresponds to the explanation above. The pipe expression `<exp1> |> <exp2>` is just syntactic sugar for the following block binding `<exp1>` to a reserved placeholder identifier, `p`, before returning `<exp2>`:

``` bnf
do { let p = <exp1>; <exp2> }
```

The otherwise inaccessible placeholder identifier `p` can only referenced by the placeholder expression `_`. Multiple references to `_` are allowed and refer to the same value within the same pipe operation.

Note that using `_` as an expression outside of a pipe operation, where it is undefined, is an error.

For example, the following example produces the compile-time error "type error [M0057], unbound variable _":

``` motoko no-repl
let x = _;
```

Internally, the compiler uses the reserved identifier `_` as the name for the placeholder called `p` above, so this `let` is just referencing an undefined variable.


See [the language manual page on pipes](../reference/language-manual#pipe-operators-and-placeholder-expressions) for more details.

<img src="https://github.com/user-attachments/assets/844ca364-4d71-42b3-aaec-4a6c3509ee2e" alt="Logo" width="150" height="150" />