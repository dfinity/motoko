---
sidebar_position: 5
---

# Characters & strings

## Characters

The `Char` type in Motoko represents a single unicode character delimited with a single quotation mark (`'`).

```motoko no-repl
let letter : Char = 'A';
let symbol : Char = '✮';

// Comparing characters
'I' == 'i' // False
```

You can convert a `Text` value `t` to a `Char` iterator using either `t.chars()` or, equivalently, `Text.toIter(t)`.

```motoko no-repl
import Text "mo:base/Text";
import Iter "mo:base/Iter";
import Char "mo:base/Char";

actor {
  // Internal function that works with iterators
  func textToChars(t: Text) : Iter.Iter<Char> {
    Text.toIter(t)
  };
  
  // Public function that demonstrates using the iterator
  public func demonstrateTextToChars(t: Text) : async Text {
    let charIter = textToChars(t);
    var result = "";
    
    // Iterate through the characters and build a result string
    for (c in charIter) {
      result #= Char.toText(c) # "";
    };
    
    return result
  };
}
```

## Strings

Sequences of characters are handled using the [`Text`](https://internetcomputer.org/docs/motoko/base/Text) type, which represents immutable strings of unicode characters delimited with a double quotation mark (`"`).

```motoko no-repl
let greeting : Text = "Hello, world!";

// Concatenating text

"ICP" # "x" # "Motoko" // ICP x Motoko
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

### Resources

- [`Char`](https://internetcomputer.org/docs/motoko/base/Char)
- [`Text`](https://internetcomputer.org/docs/motoko/base/Text)

<img src="https://cdn-assets-eu.frontify.com/s3/frontify-enterprise-files-eu/eyJwYXRoIjoiZGZpbml0eVwvYWNjb3VudHNcLzAxXC80MDAwMzA0XC9wcm9qZWN0c1wvNFwvYXNzZXRzXC8zOFwvMTc2XC9jZGYwZTJlOTEyNDFlYzAzZTQ1YTVhZTc4OGQ0ZDk0MS0xNjA1MjIyMzU4LnBuZyJ9:dfinity:9Q2_9PEsbPqdJNAQ08DAwqOenwIo7A8_tCN4PSSWkAM?width=2400" alt="Logo" width="150" height="150" />