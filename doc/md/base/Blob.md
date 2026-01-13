# base/Blob
`Blob` is an immutable, iterable sequence of bytes. Unlike `[Nat8]`, which is less compact (using 4 bytes per logical byte), `Blob` provides a more efficient representation.

Blobs are not indexable and can be empty. To manipulate a `Blob`, convert it to `[var Nat8]` or `Buffer<Nat8>`, perform your changes, then convert it back.

Import from the base library to use this module.

```motoko name=import
import Blob "mo:base/Blob";
```

:::note Additional features

Some built-in features are not listed in this module:

- You can create a `Blob` literal from a `Text` literal, provided the context expects an expression of type `Blob`.
- `b.size() : Nat` returns the number of bytes in the blob `b`.
- `b.vals() : Iter.Iter<Nat8>` returns an iterator to enumerate the bytes of the blob `b`.
:::

For example:

```motoko include=import
import Debug "mo:base/Debug";
import Nat8 "mo:base/Nat8";

let blob = "\00\00\00\ff" : Blob; // blob literals, where each byte is delimited by a back-slash and represented in hex
let blob2 = "charsもあり" : Blob; // you can also use characters in the literals
let numBytes = blob.size(); // => 4 (returns the number of bytes in the Blob)
for (byte : Nat8 in blob.vals()) { // iterator over the Blob
 Debug.print(Nat8.toText(byte))
}
```
:::note Operator limitation

Comparison functions (`equal`, `notEqual`, `less`, `lessOrEqual`, `greater`, `greaterOrEqual`) are defined in this library to allow their use as function values in higher-order functions.
Operators like `==`, `!=`, `<`, `<=`, `>`, and `>=` cannot currently be passed as function values.
:::

## Type `Blob`
``` motoko no-repl
type Blob = Prim.Types.Blob
```


## Function `fromArray`
``` motoko no-repl
func fromArray(bytes : [Nat8]) : Blob
```

Creates a `Blob` from an array of bytes (`[Nat8]`) by copying each element.

Example:
```motoko include=import
let bytes : [Nat8] = [0, 255, 0];
let blob = Blob.fromArray(bytes); // => "\00\FF\00"
```

## Function `fromArrayMut`
``` motoko no-repl
func fromArrayMut(bytes : [var Nat8]) : Blob
```

Creates a `Blob` from a mutable array of bytes (`[var Nat8]`) by copying each element.

Example:
```motoko include=import
let bytes : [var Nat8] = [var 0, 255, 0];
let blob = Blob.fromArrayMut(bytes); // => "\00\FF\00"
```

## Function `toArray`
``` motoko no-repl
func toArray(blob : Blob) : [Nat8]
```

Converts a `Blob` to an array of bytes (`[Nat8]`) by copying each element.

Example:
```motoko include=import
let blob = "\00\FF\00" : Blob;
let bytes = Blob.toArray(blob); // => [0, 255, 0]
```

## Function `toArrayMut`
``` motoko no-repl
func toArrayMut(blob : Blob) : [var Nat8]
```

Converts a `Blob` to a mutable array of bytes (`[var Nat8]`) by copying each element.

Example:
```motoko include=import
let blob = "\00\FF\00" : Blob;
let bytes = Blob.toArrayMut(blob); // => [var 0, 255, 0]
```

## Function `hash`
``` motoko no-repl
func hash(blob : Blob) : Nat32
```

Returns the (non-cryptographic) hash of `blob`.

Example:
```motoko include=import
let blob = "\00\FF\00" : Blob;
Blob.hash(blob) // => 1_818_567_776
```

## Function `compare`
``` motoko no-repl
func compare(b1 : Blob, b2 : Blob) : {#less; #equal; #greater}
```

General purpose comparison function for `Blob` by comparing the value of
the bytes. Returns the `Order` (either `#less`, `#equal`, or `#greater`)
by comparing `blob1` with `blob2`.

Example:
```motoko include=import
let blob1 = "\00\00\00" : Blob;
let blob2 = "\00\FF\00" : Blob;
Blob.compare(blob1, blob2) // => #less
```

## Function `equal`
``` motoko no-repl
func equal(blob1 : Blob, blob2 : Blob) : Bool
```

Equality function for `Blob` types.
This is equivalent to `blob1 == blob2`.

Example:
```motoko include=import
let blob1 = "\00\FF\00" : Blob;
let blob2 = "\00\FF\00" : Blob;
ignore Blob.equal(blob1, blob2);
blob1 == blob2 // => true
```


## Function `notEqual`
``` motoko no-repl
func notEqual(blob1 : Blob, blob2 : Blob) : Bool
```

Inequality function for `Blob` types.
This is equivalent to `blob1 != blob2`.

Example:
```motoko include=import
let blob1 = "\00\AA\AA" : Blob;
let blob2 = "\00\FF\00" : Blob;
ignore Blob.notEqual(blob1, blob2);
blob1 != blob2 // => true
```

## Function `less`
``` motoko no-repl
func less(blob1 : Blob, blob2 : Blob) : Bool
```

"Less than" function for `Blob` types.
This is equivalent to `blob1 < blob2`.

Example:
```motoko include=import
let blob1 = "\00\AA\AA" : Blob;
let blob2 = "\00\FF\00" : Blob;
ignore Blob.less(blob1, blob2);
blob1 < blob2 // => true
```

## Function `lessOrEqual`
``` motoko no-repl
func lessOrEqual(blob1 : Blob, blob2 : Blob) : Bool
```

"Less than or equal to" function for `Blob` types.
This is equivalent to `blob1 <= blob2`.

Example:
```motoko include=import
let blob1 = "\00\AA\AA" : Blob;
let blob2 = "\00\FF\00" : Blob;
ignore Blob.lessOrEqual(blob1, blob2);
blob1 <= blob2 // => true
```

## Function `greater`
``` motoko no-repl
func greater(blob1 : Blob, blob2 : Blob) : Bool
```

"Greater than" function for `Blob` types.
This is equivalent to `blob1 > blob2`.

Example:
```motoko include=import
let blob1 = "\BB\AA\AA" : Blob;
let blob2 = "\00\00\00" : Blob;
ignore Blob.greater(blob1, blob2);
blob1 > blob2 // => true
```

## Function `greaterOrEqual`
``` motoko no-repl
func greaterOrEqual(blob1 : Blob, blob2 : Blob) : Bool
```

"Greater than or equal to" function for `Blob` types.
This is equivalent to `blob1 >= blob2`.

Example:
```motoko include=import
let blob1 = "\BB\AA\AA" : Blob;
let blob2 = "\00\00\00" : Blob;
ignore Blob.greaterOrEqual(blob1, blob2);
blob1 >= blob2 // => true
```
