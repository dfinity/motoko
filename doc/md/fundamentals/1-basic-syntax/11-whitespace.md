---
sidebar_position: 11
---

# Whitespace

Whitespace characters (spaces, tabs, newlines) are generally ignored in Motoko, but are essential for separating syntax components like keywords and identifiers. Proper use of whitespace enhances code readability.

### Without proper whitespace

```motoko no-repl
actor Counter{public func inc(x:Int): async Int{return x+=1}} await Counter.inc(1);
```

### With proper whitespace

```motoko no-repl
actor Counter {
  public func inc(x: Int) : async Int {
    return x+=1;
  };
};

await Counter.inc(1); //2
```

## Resources

- [Motoko style guide](https://internetcomputer.org/docs/motoko/motoko-style)

<img src="https://github.com/user-attachments/assets/844ca364-4d71-42b3-aaec-4a6c3509ee2e" alt="Logo" width="150" height="150" />