---
sidebar_position: 7
---

# Text

Sequences of characters are handled using the `Text` type, which represents immutable strings of unicode characters (`"`) delimited.
<!---
Notes:
Insert link to base library reference
-->
```motoko
let greeting: Text = "Hello, world!";
```

Concatenating text

```motoko
"ICP" # "x" # "Motoko" //ICP x Motoko
```

## References

- [Text](https://internetcomputer.org/docs/current/motoko/main/base/Text)
