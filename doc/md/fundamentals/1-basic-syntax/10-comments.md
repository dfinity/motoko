---
sidebar_position: 10
---

# Comments

Motoko supports single-line, multi-line, and nested comments.

## Single line

Use `//` for comments that extend to the end of a line.

```motoko no-repl
// This is a single-line comment
```

Use `///` for function or module documentation (also known as "doc comments"). Module documentation can be exported into documentation files such as Markdown or HTML using [mo-doc](../../motoko-tooling/3-mo-doc.md).

```motoko no-repl
/// Returns the sum of two integers.
func add(a : Int, b : Int) : Int {
  a + b
}
```

## Multi-line

Use `/* ... */` for block comments spanning multiple lines.

```motoko no-repl
/* This is a
    multi-line comment */
```

## Nested

Multi-line comments can be nested within each other.

```motoko no-repl
/* Outer comment
    /* Nested comment */
    End of outer comment */
```

## Resources

- [Comment style guide](../../14-style.md#comments)

- [Generating Motoko documentation](../../motoko-tooling/3-mo-doc.md)

