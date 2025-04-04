# Text
Utility functions for `Text` values.

A `Text` value represents human-readable text as a sequence of characters of type `Char`.

```motoko
let text = "Hello!";
let size = text.size(); // 6
let iter = text.chars(); // iterator ('H', 'e', 'l', 'l', 'o', '!')
let concat = text # " 👋"; // "Hello! 👋"
```

The `"mo:base/Text"` module defines additional operations on `Text` values.

Import the module from the base library:

```motoko name=import
import Text "mo:base/Text";
```

Note: `Text` values are represented as ropes of UTF-8 character sequences with O(1) concatenation.


## Type `Text`
``` motoko no-repl
type Text = Prim.Types.Text
```

The type corresponding to primitive `Text` values.

```motoko
let hello = "Hello!";
let emoji = "👋";
let concat = hello # " " # emoji; // "Hello! 👋"
```

## Value `fromChar`
``` motoko no-repl
let fromChar : (c : Char) -> Text
```

Converts the given `Char` to a `Text` value.

```motoko include=import
let text = Text.fromChar('A'); // "A"
```

## Function `fromArray`
``` motoko no-repl
func fromArray(a : [Char]) : Text
```

Converts the given `[Char]` to a `Text` value.

```motoko include=import
let text = Text.fromArray(['A', 'v', 'o', 'c', 'a', 'd', 'o']); // "Avocado"
```

Runtime: O(a.size())
Space: O(a.size())

## Function `fromVarArray`
``` motoko no-repl
func fromVarArray(a : [var Char]) : Text
```

Converts the given `[var Char]` to a `Text` value.

```motoko include=import
let text = Text.fromVarArray([var 'E', 'g', 'g', 'p', 'l', 'a', 'n', 't']); // "Eggplant"
```

Runtime: O(a.size())
Space: O(a.size())

## Function `toIter`
``` motoko no-repl
func toIter(t : Text) : Iter.Iter<Char>
```

Iterates over each `Char` value in the given `Text`.

Equivalent to calling the `t.chars()` method where `t` is a `Text` value.

```motoko include=import
import { print } "mo:base/Debug";

for (c in Text.toIter("abc")) {
  print(debug_show c);
}
```

## Function `toArray`
``` motoko no-repl
func toArray(t : Text) : [Char]
```

Creates a new `Array` containing characters of the given `Text`.

Equivalent to `Iter.toArray(t.chars())`.

```motoko include=import
assert Text.toArray("Café") == ['C', 'a', 'f', 'é'];
```

Runtime: O(t.size())
Space: O(t.size())

## Function `toVarArray`
``` motoko no-repl
func toVarArray(t : Text) : [var Char]
```

Creates a new mutable `Array` containing characters of the given `Text`.

Equivalent to `Iter.toArrayMut(t.chars())`.

```motoko include=import
assert Text.toVarArray("Café") == [var 'C', 'a', 'f', 'é'];
```

Runtime: O(t.size())
Space: O(t.size())

## Function `fromIter`
``` motoko no-repl
func fromIter(cs : Iter.Iter<Char>) : Text
```

Creates a `Text` value from a `Char` iterator.

```motoko include=import
let text = Text.fromIter(['a', 'b', 'c'].vals()); // "abc"
```

## Function `fromList`
``` motoko no-repl
func fromList(cs : List.List<Char>) : Text
```

Create a text from a character list.
Example:
```motoko include=initialize
fromList(?('H', ?('e', ?('l', ?('l', ?('o', null))))));
// => "Hello"
```

Runtime: O(size cs)
Space: O(size cs)

## Function `toList`
``` motoko no-repl
func toList(t : Text) : List.List<Char>
```

Create a character list from a text.
Example:
```motoko include=initialize
toList("Hello");
// => ?('H', ?('e', ?('l', ?('l', ?('o', null)))))
```

Runtime: O(t.size())
Space: O(t.size())

## Function `size`
``` motoko no-repl
func size(t : Text) : Nat
```

Returns the number of characters in the given `Text`.

Equivalent to calling `t.size()` where `t` is a `Text` value.

```motoko include=import
let size = Text.size("abc"); // 3
```

## Function `hash`
``` motoko no-repl
func hash(t : Text) : Hash.Hash
```

Returns a hash obtained by using the `djb2` algorithm ([more details](http://www.cse.yorku.ca/~oz/hash.html)).

```motoko include=import
let hash = Text.hash("abc");
```

Note: this algorithm is intended for use in data structures rather than as a cryptographic hash function.

## Function `concat`
``` motoko no-repl
func concat(t1 : Text, t2 : Text) : Text
```

Returns `t1 # t2`, where `#` is the `Text` concatenation operator.

```motoko include=import
let a = "Hello";
let b = "There";
let together = a # b; // "HelloThere"
let withSpace = a # " " # b; // "Hello There"
let togetherAgain = Text.concat(a, b); // "HelloThere"
```

## Function `equal`
``` motoko no-repl
func equal(t1 : Text, t2 : Text) : Bool
```

Returns `t1 == t2`.

## Function `notEqual`
``` motoko no-repl
func notEqual(t1 : Text, t2 : Text) : Bool
```

Returns `t1 != t2`.

## Function `less`
``` motoko no-repl
func less(t1 : Text, t2 : Text) : Bool
```

Returns `t1 < t2`.

## Function `lessOrEqual`
``` motoko no-repl
func lessOrEqual(t1 : Text, t2 : Text) : Bool
```

Returns `t1 <= t2`.

## Function `greater`
``` motoko no-repl
func greater(t1 : Text, t2 : Text) : Bool
```

Returns `t1 > t2`.

## Function `greaterOrEqual`
``` motoko no-repl
func greaterOrEqual(t1 : Text, t2 : Text) : Bool
```

Returns `t1 >= t2`.

## Function `compare`
``` motoko no-repl
func compare(t1 : Text, t2 : Text) : {#less; #equal; #greater}
```

Compares `t1` and `t2` lexicographically.

```motoko include=import
import { print } "mo:base/Debug";

print(debug_show Text.compare("abc", "abc")); // #equal
print(debug_show Text.compare("abc", "def")); // #less
print(debug_show Text.compare("abc", "ABC")); // #greater
```

## Function `join`
``` motoko no-repl
func join(sep : Text, ts : Iter.Iter<Text>) : Text
```

Join an iterator of `Text` values with a given delimiter.

```motoko include=import
let joined = Text.join(", ", ["a", "b", "c"].vals()); // "a, b, c"
```

## Function `map`
``` motoko no-repl
func map(t : Text, f : Char -> Char) : Text
```

Applies a function to each character in a `Text` value, returning the concatenated `Char` results.

```motoko include=import
// Replace all occurrences of '?' with '!'
let result = Text.map("Motoko?", func(c) {
  if (c == '?') '!'
  else c
});
```

## Function `translate`
``` motoko no-repl
func translate(t : Text, f : Char -> Text) : Text
```

Returns the result of applying `f` to each character in `ts`, concatenating the intermediate text values.

```motoko include=import
// Replace all occurrences of '?' with "!!"
let result = Text.translate("Motoko?", func(c) {
  if (c == '?') "!!"
  else Text.fromChar(c)
}); // "Motoko!!"
```

## Type `Pattern`
``` motoko no-repl
type Pattern = {#char : Char; #text : Text; #predicate : (Char -> Bool)}
```

A pattern `p` describes a sequence of characters. A pattern has one of the following forms:

* `#char c` matches the single character sequence, `c`.
* `#text t` matches multi-character text sequence `t`.
* `#predicate p` matches any single character sequence `c` satisfying predicate `p(c)`.

A _match_ for `p` is any sequence of characters matching the pattern `p`.

```motoko include=import
let charPattern = #char 'A';
let textPattern = #text "phrase";
let predicatePattern : Text.Pattern = #predicate (func(c) { c == 'A' or c == 'B' }); // matches "A" or "B"
```

## Function `split`
``` motoko no-repl
func split(t : Text, p : Pattern) : Iter.Iter<Text>
```

Splits the input `Text` with the specified `Pattern`.

Two fields are separated by exactly one match.

```motoko include=import
let words = Text.split("This is a sentence.", #char ' ');
Text.join("|", words) // "This|is|a|sentence."
```

## Function `tokens`
``` motoko no-repl
func tokens(t : Text, p : Pattern) : Iter.Iter<Text>
```

Returns a sequence of tokens from the input `Text` delimited by the specified `Pattern`, derived from start to end.
A "token" is a non-empty maximal subsequence of `t` not containing a match for pattern `p`.
Two tokens may be separated by one or more matches of `p`.

```motoko include=import
let tokens = Text.tokens("this needs\n an   example", #predicate (func(c) { c == ' ' or c == '\n' }));
Text.join("|", tokens) // "this|needs|an|example"
```

## Function `contains`
``` motoko no-repl
func contains(t : Text, p : Pattern) : Bool
```

Returns `true` if the input `Text` contains a match for the specified `Pattern`.

```motoko include=import
Text.contains("Motoko", #text "oto") // true
```

## Function `startsWith`
``` motoko no-repl
func startsWith(t : Text, p : Pattern) : Bool
```

Returns `true` if the input `Text` starts with a prefix matching the specified `Pattern`.

```motoko include=import
Text.startsWith("Motoko", #text "Mo") // true
```

## Function `endsWith`
``` motoko no-repl
func endsWith(t : Text, p : Pattern) : Bool
```

Returns `true` if the input `Text` ends with a suffix matching the specified `Pattern`.

```motoko include=import
Text.endsWith("Motoko", #char 'o') // true
```

## Function `replace`
``` motoko no-repl
func replace(t : Text, p : Pattern, r : Text) : Text
```

Returns the input text `t` with all matches of pattern `p` replaced by text `r`.

```motoko include=import
let result = Text.replace("abcabc", #char 'a', "A"); // "AbcAbc"
```

## Function `stripStart`
``` motoko no-repl
func stripStart(t : Text, p : Pattern) : ?Text
```

Strips one occurrence of the given `Pattern` from the beginning of the input `Text`.
If you want to remove multiple instances of the pattern, use `Text.trimStart()` instead.

```motoko include=import
// Try to strip a nonexistent character
let none = Text.stripStart("abc", #char '-'); // null
// Strip just one '-'
let one = Text.stripStart("--abc", #char '-'); // ?"-abc"
```

## Function `stripEnd`
``` motoko no-repl
func stripEnd(t : Text, p : Pattern) : ?Text
```

Strips one occurrence of the given `Pattern` from the end of the input `Text`.
If you want to remove multiple instances of the pattern, use `Text.trimEnd()` instead.

```motoko include=import
// Try to strip a nonexistent character
let none = Text.stripEnd("xyz", #char '-'); // null
// Strip just one '-'
let one = Text.stripEnd("xyz--", #char '-'); // ?"xyz-"
```

## Function `trimStart`
``` motoko no-repl
func trimStart(t : Text, p : Pattern) : Text
```

Trims the given `Pattern` from the start of the input `Text`.
If you only want to remove a single instance of the pattern, use `Text.stripStart()` instead.

```motoko include=import
let trimmed = Text.trimStart("---abc", #char '-'); // "abc"
```

## Function `trimEnd`
``` motoko no-repl
func trimEnd(t : Text, p : Pattern) : Text
```

Trims the given `Pattern` from the end of the input `Text`.
If you only want to remove a single instance of the pattern, use `Text.stripEnd()` instead.

```motoko include=import
let trimmed = Text.trimEnd("xyz---", #char '-'); // "xyz"
```

## Function `trim`
``` motoko no-repl
func trim(t : Text, p : Pattern) : Text
```

Trims the given `Pattern` from both the start and end of the input `Text`.

```motoko include=import
let trimmed = Text.trim("---abcxyz---", #char '-'); // "abcxyz"
```

## Function `compareWith`
``` motoko no-repl
func compareWith(t1 : Text, t2 : Text, cmp : (Char, Char) -> {#less; #equal; #greater}) : {#less; #equal; #greater}
```

Compares `t1` and `t2` using the provided character-wise comparison function.

```motoko include=import
import Char "mo:base/Char";

Text.compareWith("abc", "ABC", func(c1, c2) { Char.compare(c1, c2) }) // #greater
```

## Value `encodeUtf8`
``` motoko no-repl
let encodeUtf8 : Text -> Blob
```

Returns a UTF-8 encoded `Blob` from the given `Text`.

```motoko include=import
let blob = Text.encodeUtf8("Hello");
```

## Value `decodeUtf8`
``` motoko no-repl
let decodeUtf8 : Blob -> ?Text
```

Tries to decode the given `Blob` as UTF-8.
Returns `null` if the blob is not valid UTF-8.

```motoko include=import
let text = Text.decodeUtf8("\48\65\6C\6C\6F"); // ?"Hello"
```

## Value `toLowercase`
``` motoko no-repl
let toLowercase : Text -> Text
```

Returns the text argument in lowercase.
WARNING: Unicode compliant only when compiled, not interpreted.

```motoko include=import
let text = Text.toLowercase("Good Day"); // ?"good day"
```

## Value `toUppercase`
``` motoko no-repl
let toUppercase : Text -> Text
```

Returns the text argument in uppercase. Unicode compliant.
WARNING: Unicode compliant only when compiled, not interpreted.

```motoko include=import
let text = Text.toUppercase("Good Day"); // ?"GOOD DAY"
```
