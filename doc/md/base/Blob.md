# Blob
Binary blobs

## Type `Blob`
``` motoko norepl
type Blob = Prim.Types.Blob
```

An immutable, possibly empty sequence of bytes.
Given `b : Blob`:

* `b.size() : Nat` returns the number of bytes in the blob;
* `b.vals() : Iter.Iter<Nat8>` returns an iterator to enumerate the bytes of the blob.

(Direct indexing of Blobs is not yet supported.)

## Value `hash`
``` motoko norepl
let hash : (b : Blob) -> Nat32
```

Returns a (non-cryptographic) hash of 'b'

## Function `equal`
``` motoko norepl
func equal(x : Blob, y : Blob) : Bool
```

Returns `x == y`.

## Function `notEqual`
``` motoko norepl
func notEqual(x : Blob, y : Blob) : Bool
```

Returns `x != y`.

## Function `less`
``` motoko norepl
func less(x : Blob, y : Blob) : Bool
```

Returns `x < y`.

## Function `lessOrEqual`
``` motoko norepl
func lessOrEqual(x : Blob, y : Blob) : Bool
```

Returns `x <= y`.

## Function `greater`
``` motoko norepl
func greater(x : Blob, y : Blob) : Bool
```

Returns `x > y`.

## Function `greaterOrEqual`
``` motoko norepl
func greaterOrEqual(x : Blob, y : Blob) : Bool
```

Returns `x >= y`.

## Function `compare`
``` motoko norepl
func compare(x : Blob, y : Blob) : {#less; #equal; #greater}
```

Returns the order of `x` and `y`.

## Value `fromArray`
``` motoko norepl
let fromArray : [Nat8] -> Blob
```

Creates a blob from an array of bytes, by copying each element.

## Value `fromArrayMut`
``` motoko norepl
let fromArrayMut : [var Nat8] -> Blob
```

Creates a blob from a mutable array of bytes, by copying each element.

## Value `toArray`
``` motoko norepl
let toArray : Blob -> [Nat8]
```

Creates an array of bytes from a blob, by copying each element.

## Value `toArrayMut`
``` motoko norepl
let toArrayMut : Blob -> [var Nat8]
```

Creates a mutable array of bytes from a blob, by copying each element.
