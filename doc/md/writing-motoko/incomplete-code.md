---
sidebar_position: 27
---
# Writing incomplete code



In the midst of writing a program, you may want to run an incomplete version or a version where one or more execution paths are either missing or simply invalid.

To accommodate these situations, Motoko provides the `xxx`, `nyi` and `unreachable` functions from the base `Prelude` library, explained below. Each is a simple wrapper around a more [general trap mechanism](../getting-started/basic-concepts.md#traps).

## Short-term holes

Assuming that one has imported the prelude as follows:

``` motoko no-repl
import P "mo:base/Prelude";
```

You can fill any missing expression with the following:

``` motoko no-repl
P.xxx()
```

The expression `xxx()` has type `None`, which is a subtype of every other type. This means the expression can plug any short-term gap in your code.
`xxx()` will trap when executed.

## Long-term holes

By convention, longer-term holes can be considered "not yet implemented" (`nyi`) features, and marked as such with a similar function from the Prelude module:

``` motoko no-repl
P.nyi()
```
Like `xxx()`, `nyi()` has type `None`, thus any other type, and will trap on execution.
## Unreachable code


Sometimes you will be required to provide code that they know will never be executed, due to the preceding program logic.

To document unreachable code, you can use the  `unreachable` function from the prelude:

``` motoko no-repl
P.unreachable()
```

Like `unreachable()` has type `None` and thus any other type, and will trap on (unexpected!) execution.

<img src="https://github.com/user-attachments/assets/844ca364-4d71-42b3-aaec-4a6c3509ee2e" alt="Logo" width="150" height="150" />