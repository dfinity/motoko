# core/Random
Random number generation.

Import from the core package to use this module.
```motoko name=import
import Random "mo:core/Random";
```

## Type `State`
``` motoko no-repl
type State = { var bytes : [Nat8]; var index : Nat; var bits : Nat8; var bitMask : Nat8 }
```


## Type `SeedState`
``` motoko no-repl
type SeedState = { random : State; prng : PRNG.State }
```


## Function `blob`
``` motoko no-repl
func blob() : async Blob
```


## Function `emptyState`
``` motoko no-repl
func emptyState() : State
```

Initializes a random number generator state. This is used
to create a `Random` or `AsyncRandom` instance with a specific state.
The state is empty, but it can be reused after upgrading the canister.

Example:
```motoko
import Random "mo:core/Random";

persistent actor {
  let state = Random.emptyState();
  transient let random = Random.cryptoFromState(state);

  public func main() : async () {
    let coin = await* random.bool(); // true or false
  }
}
```

## Function `seedState`
``` motoko no-repl
func seedState(seed : Nat64) : SeedState
```

Initializes a pseudo-random number generator state with a 64-bit seed.
This is used to create a `Random` instance with a specific seed.
The seed is used to initialize the PRNG state.

Example:
```motoko
import Random "mo:core/Random";

persistent actor {
  let state = Random.seedState(123);
  transient let random = Random.seedFromState(state);

  public func main() : async () {
    let coin = random.bool(); // true or false
  }
}
```

## Function `seed`
``` motoko no-repl
func seed(seed : Nat64) : Random
```

Creates a pseudo-random number generator from a 64-bit seed.
The seed is used to initialize the PRNG state.
This is suitable for simulations and testing, but not for cryptographic purposes.

Example:
```motoko include=import
let random = Random.seed(123);
let coin = random.bool(); // true or false
```

## Function `seedFromState`
``` motoko no-repl
func seedFromState(state : SeedState) : Random
```

Creates a pseudo-random number generator with the given state.
This provides statistical randomness suitable for simulations and testing,
but should not be used for cryptographic purposes.

Example:
```motoko
import Random "mo:core/Random";

persistent actor {
  let state = Random.seedState(123);
  transient let random = Random.seedFromState(state);

  public func main() : async () {
    let coin = random.bool(); // true or false
  }
}
```

## Function `crypto`
``` motoko no-repl
func crypto() : AsyncRandom
```

Initializes a cryptographic random number generator
using entropy from the ICP management canister.

Example:
```motoko
import Random "mo:core/Random";

persistent actor {
  transient let random = Random.crypto();

  public func main() : async () {
    let coin = await* random.bool(); // true or false
  }
}
```

## Function `cryptoFromState`
``` motoko no-repl
func cryptoFromState(state : State) : AsyncRandom
```

Creates a random number generator suitable for cryptography
using entropy from the ICP management canister. Initializing
from a state makes it possible to reuse entropy after
upgrading the canister.

Example:
```motoko
import Random "mo:core/Random";

persistent actor {
  let state = Random.emptyState();
  transient let random = Random.cryptoFromState(state);

  func example() : async () {
    let coin = await* random.bool(); // true or false
  }
}
```

## Class `Random`

``` motoko no-repl
class Random(state : State, generator : () -> Blob)
```


### Function `bool`
``` motoko no-repl
func bool() : Bool
```

Random choice between `true` and `false`.

Example:
```motoko include=import
let random = Random.seed(42);
let coin = random.bool(); // true or false
```


### Function `nat8`
``` motoko no-repl
func nat8() : Nat8
```

Random `Nat8` value in the range [0, 256).

Example:
```motoko include=import
let random = Random.seed(42);
let byte = random.nat8(); // 0 to 255
```


### Function `nat64`
``` motoko no-repl
func nat64() : Nat64
```

Random `Nat64` value in the range [0, 2^64).

Example:
```motoko include=import
let random = Random.seed(42);
let number = random.nat64(); // 0 to 18446744073709551615
```


### Function `nat64Range`
``` motoko no-repl
func nat64Range(fromInclusive : Nat64, toExclusive : Nat64) : Nat64
```

Random `Nat64` value in the range [fromInclusive, toExclusive).

Example:
```motoko include=import
let random = Random.seed(42);
let dice = random.nat64Range(1, 7); // 1 to 6
```


### Function `natRange`
``` motoko no-repl
func natRange(fromInclusive : Nat, toExclusive : Nat) : Nat
```

Random `Nat` value in the range [fromInclusive, toExclusive).

Example:
```motoko include=import
let random = Random.seed(42);
let index = random.natRange(0, 10); // 0 to 9
```


### Function `intRange`
``` motoko no-repl
func intRange(fromInclusive : Int, toExclusive : Int) : Int
```


## Class `AsyncRandom`

``` motoko no-repl
class AsyncRandom(state : State, generator : () -> async* Blob)
```


### Function `bool`
``` motoko no-repl
func bool() : async* Bool
```

Random choice between `true` and `false`.


### Function `nat8`
``` motoko no-repl
func nat8() : async* Nat8
```

Random `Nat8` value in the range [0, 256).


### Function `nat64`
``` motoko no-repl
func nat64() : async* Nat64
```

Random `Nat64` value in the range [0, 2^64).


### Function `nat64Range`
``` motoko no-repl
func nat64Range(fromInclusive : Nat64, toExclusive : Nat64) : async* Nat64
```

Random `Nat64` value in the range [fromInclusive, toExclusive).


### Function `natRange`
``` motoko no-repl
func natRange(fromInclusive : Nat, toExclusive : Nat) : async* Nat
```

Random `Nat` value in the range [fromInclusive, toExclusive).


### Function `intRange`
``` motoko no-repl
func intRange(fromInclusive : Int, toExclusive : Int) : async* Int
```

Random `Int` value in the range [fromInclusive, toExclusive).
