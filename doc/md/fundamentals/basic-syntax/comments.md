---
sidebar_position: 10
---

# Comments  

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
