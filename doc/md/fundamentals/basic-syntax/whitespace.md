---
sidebar_position: 9
---

# Whitespace

Whitespace characters (spaces, tabs, newlines) are generally ignored in Motoko but are essential for separating tokens like keywords and identifiers. Proper use of whitespace enhances code readability.

An example without proper whitespace:

```motoko
actor Incrementer{public func inc(x:Int): async Int{return x+=1}} await Incrementer.inc(1);
```

With Proper Whitespace

```motoko
actor Incrementer {
  public func inc(x: Int) : async Int {
    return x+=1;
  };
};

await Incrementer.inc(1); //2
```
