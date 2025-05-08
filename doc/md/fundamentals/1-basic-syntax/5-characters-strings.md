---
sidebar_position: 5
---

# Characters & strings

## Characters

The `Char` type in Motoko represents a single unicode character delimited with a single quotation mark (`'`).

```motoko
let letter : Char = 'A';
let symbol : Char = '✮';

// Comparing characters
'I' == 'i' // False
```

You can convert a `Text` value `t` to a `Char` iterator using either `t.chars()` or, equivalently, `Text.toIter(t)`.

:::info [Iter](https://internetcomputer.org/docs/motoko/base/Iter)
An `Iter<T>` is an object that sequentially produces values of specified type `T` until no more values remain.
:::

```motoko
import Text "mo:base/Text";
import Iter "mo:base/Iter";
import Char "mo:base/Char";

persistent actor Alternator {

  // Turn text into an iterator of Char
  func textToChars(t: Text) : Iter.Iter<Char> {
    t.chars();
  };

  // Alternate capitalization
  public func alternateCaps(t: Text) : async Text {
    let chars = textToChars(t);
    var index = 0;
    // Apply a case function to each char
    let modified = Iter.map<Char, Text>(chars, func(c: Char) : Text {
      let charAsText = Char.toText(c);
      let transformedText = 
        if (index % 2 == 0) {
          Text.toUppercase(charAsText)
        } else {
          Text.toLowercase(charAsText)
        };
      index += 1;
      transformedText;
    });

    return Text.join("", modified);
  };
};
await Alternator.alternateCaps("motoko"); 
```

:::note Conversions

- `Char` can be converted to a single-character `Text` using `Text.fromChar(c)`.
- `Text` can be converted to a single-character `Char` using `Char.toText(t)`.
:::

## Strings

Sequences of characters are handled using the [`Text`](https://internetcomputer.org/docs/motoko/base/Text) type, which represents immutable strings of unicode characters delimited with a double quotation mark (`"`).

```motoko
let greeting : Text = "Hello, world!";

// Concatenating text

"ICP " # "❤️" # " Motoko" // "ICP ❤️ Motoko"
```

`t.size()` returns the number of characters in the text `t`.  

```motoko
"abc".size() == 3
```

`t.chars()` returns an iterator enumerating the characters in `t`. For example:  

```motoko  
import Char "mo:base/Char";  

for (c in "abc".chars()) {  
  Debug.print(Char.toText(c));  
} 
```

Text values can be compared using "==", "<" and all the other relational operators.

## Resources

- [`Char`](https://internetcomputer.org/docs/motoko/base/Char)
- [`Text`](https://internetcomputer.org/docs/motoko/base/Text)
- [`Iter`](https://internetcomputer.org/docs/motoko/base/Iter)

<img src="https://cdn-assets-eu.frontify.com/s3/frontify-enterprise-files-eu/eyJwYXRoIjoiZGZpbml0eVwvYWNjb3VudHNcLzAxXC80MDAwMzA0XC9wcm9qZWN0c1wvNFwvYXNzZXRzXC8zOFwvMTc2XC9jZGYwZTJlOTEyNDFlYzAzZTQ1YTVhZTc4OGQ0ZDk0MS0xNjA1MjIyMzU4LnBuZyJ9:dfinity:9Q2_9PEsbPqdJNAQ08DAwqOenwIo7A8_tCN4PSSWkAM?width=2400" alt="Logo" width="150" height="150" />