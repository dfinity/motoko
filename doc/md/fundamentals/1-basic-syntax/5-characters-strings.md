---
sidebar_position: 5
---

# Characters & strings

## Characters

The `Char` type in Motoko represents a single unicode character delimited with a single quotation mark (`'`).

```motoko no-repl
let letter: Char = 'A';
let symbol: Char = '✮';

// Comparing characters
'I' == 'i' // False
```

### References

- [Char](/docs/motoko/base/Char)

## Strings

Sequences of characters are handled using the [`Text`](/docs/motoko/base/Text) type, which represents immutable strings of unicode characters delimited with a double quotation mark (`"`).

```motoko no-repl
let greeting: Text = "Hello, world!";

// Concatenating text

"ICP" # "x" # "Motoko" // ICP x Motoko
```

### References

- [Text](/docs/motoko/base/Text)

<img src="https://github.com/user-attachments/assets/844ca364-4d71-42b3-aaec-4a6c3509ee2e" alt="Logo" width="150" height="150" />