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

## Strings

Sequences of characters are handled using the [`Text`](https://internetcomputer.org/docs/motoko/base/Text) type, which represents immutable strings of unicode characters delimited with a double quotation mark (`"`).

```motoko no-repl
let greeting: Text = "Hello, world!";

// Concatenating text

"ICP" # "x" # "Motoko" // ICP x Motoko
```

### Resources

- [`Char`](https://internetcomputer.org/docs/motoko/base/Char)
- [`Text`](https://internetcomputer.org/docs/motoko/base/Text)

<img src="https://cdn-assets-eu.frontify.com/s3/frontify-enterprise-files-eu/eyJwYXRoIjoiZGZpbml0eVwvYWNjb3VudHNcLzAxXC80MDAwMzA0XC9wcm9qZWN0c1wvNFwvYXNzZXRzXC8zOFwvMTc2XC9jZGYwZTJlOTEyNDFlYzAzZTQ1YTVhZTc4OGQ0ZDk0MS0xNjA1MjIyMzU4LnBuZyJ9:dfinity:9Q2_9PEsbPqdJNAQ08DAwqOenwIo7A8_tCN4PSSWkAM?width=2400" alt="Logo" width="150" height="150" />