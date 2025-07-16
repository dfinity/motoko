# core/internal/PRNG
Collection of pseudo-random number generators

The algorithms deliver deterministic statistical randomness,
not cryptographic randomness.

Algorithm 1: 128-bit Seiran PRNG
See: https://github.com/andanteyk/prng-seiran

Algorithm 2: SFC64 and SFC32 (Chris Doty-Humphrey’s Small Fast Chaotic PRNG)
See: https://numpy.org/doc/stable/reference/random/bit_generators/sfc64.html

Copyright: 2023 MR Research AG
Main author: react0r-com
Contributors: Timo Hanke (timohanke)

## Class `SFC64`

``` motoko no-repl
class SFC64(p : Nat64, q : Nat64, r : Nat64)
```

Constructs an SFC 64-bit generator.
The recommended constructor arguments are: 24, 11, 3.

Example:
```motoko
import PRNG "mo:core/internal/PRNG";

let rng = PRNG.SFC64(24, 11, 3);
```
For convenience, the function `SFC64a()` returns a generator constructed
with the recommended parameter set (24, 11, 3).

### Function `init`
``` motoko no-repl
func init(seed : Nat64)
```

Initializes the PRNG state with a particular seed

Example:
```motoko


### Function `initPre`
``` motoko no-repl
func initPre()
```

Initializes the PRNG state with a hardcoded seed.
No argument is required.

Example:


### Function `init3`
``` motoko no-repl
func init3(seed1 : Nat64, seed2 : Nat64, seed3 : Nat64)
```

Initializes the PRNG state with three state variables

Example:


### Function `next`
``` motoko no-repl
func next() : Nat64
```

Returns one output and advances the PRNG's state

Example:

## Function `sfc64a`
``` motoko no-repl
func sfc64a() : SFC64
```

SFC64a is the same as numpy.
See: [sfc64_next()](https:///github.com/numpy/numpy/blob/b6d372c25fab5033b828dd9de551eb0b7fa55800/numpy/random/src/sfc64/sfc64.h#L28)
