# Hash
Hash values

## Type `Hash`
`type Hash = Nat32`

Hash values represent a string of _hash bits_, packed into a `Nat32`.

## Value `length`
`let length : Nat`

The hash length, always 31.

## Function `bit`
`func bit(h : Hash, pos : Nat) : Bool`

Project a given bit from the bit vector.

## Function `equal`
`func equal(ha : Hash, hb : Hash) : Bool`

Test if two hashes are equal

## Function `hash`
`func hash(n : Nat) : Hash`

Computes a hash from the least significant 32-bits of `n`, ignoring other bits.
@deprecated For large `Nat` values consider using a bespoke hash function that considers all of the argument's bits.

## Function `debugPrintBits`
`func debugPrintBits(bits : Hash)`

@deprecated This function will be removed in future.

## Function `debugPrintBitsRev`
`func debugPrintBitsRev(bits : Hash)`

@deprecated This function will be removed in future.

## Function `hashNat8`
`func hashNat8(key : [Hash]) : Hash`

Jenkin's one at a time:

https://en.wikipedia.org/wiki/Jenkins_hash_function#one_at_a_time

The input type should actually be `[Nat8]`.
Note: Be sure to explode each `Nat8` of a `Nat32` into its own `Nat32`, and to shift into lower 8 bits.
@deprecated This function may be removed or changed in future.
