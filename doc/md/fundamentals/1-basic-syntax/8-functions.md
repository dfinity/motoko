---
sidebar_position: 8
hide_table_of_contents: true
---

# Functions

Functions in Motoko can be take on a variety of different attributes, the most basic of which is if the function is public or private. Public functions are callable by users or other [canisters](https://internetcomputer.org/docs/building-apps/essentials/canisters), while private functions can only be called within the program that defines it.

The most basic Motoko [function declaration](https://internetcomputer.org/docs/motoko/fundamentals/declarations/function-declarations) is:

```motoko
func exampleFunction() {};
```

By default, all functions are declared private unless explicitly declared `public`.

```motoko no-repl
public func exampleFunction() {};
```

A function should specify a return type return a value of that type. If a return type is not declared, it defaults to the unit `()` return type.

```motoko no-repl
public func exampleFunction(x : Nat) : Nat {
    return x;
};
```

[Learn more about functions](https://internetcomputer.org/docs/motoko/fundamentals/types/functions).

<img src="https://github.com/user-attachments/assets/844ca364-4d71-42b3-aaec-4a6c3509ee2e" alt="Logo" width="150" height="150" />