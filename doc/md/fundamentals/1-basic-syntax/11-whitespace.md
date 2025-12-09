---
sidebar_position: 11
---

# Whitespace

Whitespace characters (spaces, tabs, newlines) are generally ignored in Motoko, but are essential for separating syntax components like keywords and identifiers. Proper use of whitespace enhances code readability.

### Incorrect use of whitespace

```motoko
persistent actor Counter{var x : Nat = 0; public func inc(): async Int{x+1; }};
```

### Proper whitespace usage

```motoko
persistent actor Counter {
  var x : Nat = 0;
  public func inc() : async Int {
    x + 1;
  };
};
```

## Resources

- [Motoko style guide](../../14-style.md)

