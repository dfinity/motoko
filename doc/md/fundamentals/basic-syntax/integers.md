---
sidebar_position: 4
---

# Integers

Int: Represents all integers, both positive and negative (e.g., -2, -1, 0, 1, 2). By default, `Int` is unbounded, meaning it can grow as large as needed without overflow.

For scenarios requiring fixed-size integers, Motoko offers bounded variants with specific bit-widths (8, 16, 32, 64). These types can overflow if their limits are exceeded, resulting in a runtime error.

```motoko
let a: Int = -42;
let c: Int32 = 2147483647; // max bounded 32-bit integer
```
