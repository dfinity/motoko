# core/Random
Random number generation.

## Function `blob`
``` motoko no-repl
func blob() : async Blob
```


## Function `fast`
``` motoko no-repl
func fast(seed : Nat64) : Random
```

Creates a fast pseudo-random number generator using the SFC64 algorithm.
This provides statistical randomness suitable for simulations and testing,
but should not be used for cryptographic purposes.
The seed blob's first 8 bytes are used to initialize the PRNG.

## Function `crypto`
``` motoko no-repl
func crypto() : AsyncRandom
```

Creates a random number generator suitable for cryptography
using entropy from the ICP management canister with automatic resupply.

## Function `fromGenerator`
``` motoko no-repl
func fromGenerator(generator : () -> Blob) : Random
```


## Function `fromAsyncGenerator`
``` motoko no-repl
func fromAsyncGenerator(generator : () -> async* Blob) : AsyncRandom
```


## Class `Random`

``` motoko no-repl
class Random({ nextBit : () -> Bool; nextByte : () -> Nat8 })
```


### Value `bool`
``` motoko no-repl
let bool
```

Random choice between `true` and `false`.


### Value `nat8`
``` motoko no-repl
let nat8
```

Random `Nat8` value in the range [0, 256).


### Function `nat64`
``` motoko no-repl
func nat64() : Nat64
```



### Function `nat64Range`
``` motoko no-repl
func nat64Range(fromInclusive : Nat64, toExclusive : Nat64) : Nat64
```



### Function `natRange`
``` motoko no-repl
func natRange(fromInclusive : Nat, toExclusive : Nat) : Nat
```



### Function `intRange`
``` motoko no-repl
func intRange(fromInclusive : Int, toExclusive : Int) : Int
```


## Class `AsyncRandom`

``` motoko no-repl
class AsyncRandom({ nextBit : () -> async* Bool; nextByte : () -> async* Nat8 })
```


### Value `bool`
``` motoko no-repl
let bool
```

Random choice between `true` and `false`.


### Value `nat8`
``` motoko no-repl
let nat8
```

Random `Nat8` value in the range [0, 256).


### Function `nat64`
``` motoko no-repl
func nat64() : async* Nat64
```



### Function `nat64Range`
``` motoko no-repl
func nat64Range(fromInclusive : Nat64, toExclusive : Nat64) : async* Nat64
```



### Function `natRange`
``` motoko no-repl
func natRange(fromInclusive : Nat, toExclusive : Nat) : async* Nat
```



### Function `intRange`
``` motoko no-repl
func intRange(fromInclusive : Int, toExclusive : Int) : async* Int
```

