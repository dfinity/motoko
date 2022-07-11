# Text
Text values

This type represents human-readable text as sequences of characters of type `Char`.
If `t` is a value of type `Text`, then:

* `t.chars()` returns an _iterator_ of type `Iter<Char>` enumerating its characters from first to last.
* `t.size()` returns the _size_ (or length) of `t` (and `t.chars()`) as a `Nat`.
* `t1 # t2` concatenates texts `t1` and `t2`.

Represented as ropes of UTF-8 character sequences with O(1) concatenation.

This module defines additional operations on `Text` values.

## Type `Text`
``` motoko norepl
type Text = Prim.Types.Text
```

Text values.

## Value `fromChar`
``` motoko norepl
let fromChar : (c : Char) -> Text
```

Conversion.
Returns the text value of size 1 containing the single character `c`.

## Function `toIter`
``` motoko norepl
func toIter(t : Text) : Iter.Iter<Char>
```

Conversion.
Creates an iterator that traverses the characters of the text `t`.

## Function `fromIter`
``` motoko norepl
func fromIter(cs : Iter.Iter<Char>) : Text
```

Conversion.
Returns the text value containing the sequence of characters in `cs`.

## Function `size`
``` motoko norepl
func size(t : Text) : Nat
```

Returns `t.size()`, the number of characters in `t` (and `t.chars()`).

## Function `hash`
``` motoko norepl
func hash(t : Text) : Hash.Hash
```

Returns a hash obtained by using the `djb2` algorithm from http://www.cse.yorku.ca/~oz/hash.html

This function is _good enough_ for use in a hash-table but it's not a cryptographic hash function!

## Function `concat`
``` motoko norepl
func concat(t1 : Text, t2 : Text) : Text
```

Returns the concatenation of `t1` and `t2`, `t1 # t2`.

## Function `equal`
``` motoko norepl
func equal(t1 : Text, t2 : Text) : Bool
```

Returns `t1 == t2`.

## Function `notEqual`
``` motoko norepl
func notEqual(t1 : Text, t2 : Text) : Bool
```

Returns `t1 != t2`.

## Function `less`
``` motoko norepl
func less(t1 : Text, t2 : Text) : Bool
```

Returns `t1 < t2`.

## Function `lessOrEqual`
``` motoko norepl
func lessOrEqual(t1 : Text, t2 : Text) : Bool
```

Returns `t1 <= t2`.

## Function `greater`
``` motoko norepl
func greater(t1 : Text, t2 : Text) : Bool
```

Returns `t1 > t2`.

## Function `greaterOrEqual`
``` motoko norepl
func greaterOrEqual(t1 : Text, t2 : Text) : Bool
```

Returns `t1 >= t2`.

## Function `compare`
``` motoko norepl
func compare(t1 : Text, t2 : Text) : {#less; #equal; #greater}
```

Returns the order of `t1` and `t1`.

## Function `join`
``` motoko norepl
func join(sep : Text, ts : Iter.Iter<Text>) : Text
```

Returns the concatenation of text values in `ts`, separated by `sep`.

## Function `map`
``` motoko norepl
func map(t : Text, f : Char -> Char) : Text
```

Returns the result of applying `f` to each character in `ts`, concatenating the intermediate single-character text values.

## Function `translate`
``` motoko norepl
func translate(t : Text, f : Char -> Text) : Text
```

Returns the result of applying `f` to each character in `ts`, concatenating the intermediate text values.

## Type `Pattern`
``` motoko norepl
type Pattern = {#char : Char; #text : Text; #predicate : (Char -> Bool)}
```

A pattern `p` describes a sequence of characters. A pattern has one of the following forms:

* `#char c` matches the single character sequence, `c`.
* `#predicate p` matches any single character sequence `c` satisfying predicate `p(c)`.
* `#text t` matches multi-character text sequence `t`.

A _match_ for `p` is any sequence of characters matching the pattern `p`.

## Function `split`
``` motoko norepl
func split(t : Text, p : Pattern) : Iter.Iter<Text>
```

Returns the sequence of fields in `t`, derived from start to end,
separated by text matching pattern `p`.
Two fields are separated by exactly one match.

## Function `tokens`
``` motoko norepl
func tokens(t : Text, p : Pattern) : Iter.Iter<Text>
```

Returns the sequence of tokens in `t`, derived from start to end.
A _token_ is a non-empty maximal subsequence of `t` not containing a match for pattern `p`.
Two tokens may be separated by one or more matches of `p`.

## Function `contains`
``` motoko norepl
func contains(t : Text, p : Pattern) : Bool
```

Returns true if `t` contains a match for pattern `p`.

## Function `startsWith`
``` motoko norepl
func startsWith(t : Text, p : Pattern) : Bool
```

Returns `true` if `t` starts with a prefix matching pattern `p`, otherwise returns `false`.

## Function `endsWith`
``` motoko norepl
func endsWith(t : Text, p : Pattern) : Bool
```

Returns `true` if `t` ends with a suffix matching pattern `p`, otherwise returns `false`.

## Function `replace`
``` motoko norepl
func replace(t : Text, p : Pattern, r : Text) : Text
```

Returns `t` with all matches of pattern `p` replaced by text `r`.

## Function `stripStart`
``` motoko norepl
func stripStart(t : Text, p : Pattern) : ?Text
```

Returns the optioned suffix of `t` obtained by eliding exactly one leading match of pattern `p`, otherwise `null`.

## Function `stripEnd`
``` motoko norepl
func stripEnd(t : Text, p : Pattern) : ?Text
```

Returns the optioned prefix of `t` obtained by eliding exactly one trailing match of pattern `p`, otherwise `null`.

## Function `trimStart`
``` motoko norepl
func trimStart(t : Text, p : Pattern) : Text
```

Returns the suffix of `t` obtained by eliding all leading matches of pattern `p`.

## Function `trimEnd`
``` motoko norepl
func trimEnd(t : Text, p : Pattern) : Text
```

Returns the prefix of `t` obtained by eliding all trailing matches of pattern `p`.

## Function `trim`
``` motoko norepl
func trim(t : Text, p : Pattern) : Text
```

Returns the subtext of `t` obtained by eliding all leading and trailing matches of pattern `p`.

## Function `compareWith`
``` motoko norepl
func compareWith(t1 : Text, t2 : Text, cmp : (Char, Char) -> {#less; #equal; #greater}) : {#less; #equal; #greater}
```

Returns the lexicographic comparison of `t1` and `t2`, using the given character ordering `cmp`.

## Value `encodeUtf8`
``` motoko norepl
let encodeUtf8 : Text -> Blob
```

Returns the UTF-8 encoding of the given text

## Value `decodeUtf8`
``` motoko norepl
let decodeUtf8 : Blob -> ?Text
```

Tries to decode the given `Blob` as UTF-8.
Returns `null` if the blob is _not_ valid UTF-8.
