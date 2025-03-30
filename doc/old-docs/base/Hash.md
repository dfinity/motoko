# Hash

Hash values

## Type `Hash`

``` motoko no-repl
type Hash = Nat32
```

Hash values represent a string of _hash bits_, packed into a `Nat32`.

## Value `length`

``` motoko no-repl
let length : Nat
```

The hash length, always 31.

## Function `bit`

``` motoko no-repl
func bit(h : Hash, pos : Nat) : Bool
```

Project a given bit from the bit vector.

## Function `equal`

``` motoko no-repl
func equal(ha : Hash, hb : Hash) : Bool
```

Test if two hashes are equal.

## Function `hash` — @deprecated

``` motoko no-repl
func hash(n : Nat) : Hash
```

:::warning [Deprecated function]

This function computes a hash from the least significant 32 bits of `n`, ignoring other bits.  
For large `Nat` values, consider using a bespoke hash function that considers all of the argument's bits.
:::

## Function `debugPrintBits` — @deprecated

``` motoko no-repl
func debugPrintBits(bits : Hash)
```

:::warning [Deprecated function]

This function will be removed in a future version.
:::

## Function `debugPrintBitsRev` — @deprecated

``` motoko no-repl
func debugPrintBitsRev(bits : Hash)
```

:::warning [Deprecated function]

This function will be removed in a future version.
:::

## Function `hashNat8` — @deprecated

``` motoko no-repl
func hashNat8(key : [Hash]) : Hash
```

Jenkin's one-at-a-time hash:  
<https://en.wikipedia.org/wiki/Jenkins_hash_function#one_at_a_time>

:::note
The input type should actually be `[Nat8]`.
Be sure to explode each `Nat8` of a `Nat32` into its own `Nat32`, and shift into the lower 8 bits.
:::
:::warning [Deprecated function]

This function may be removed or changed in a future version.
:::
