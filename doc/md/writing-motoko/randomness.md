---
sidebar_position: 22
---

# Randomness



The Motoko [`Random`](../base/Random.md) base library can be used for generating random values within smart contracts on ICP. Randomness on ICP is an intricate process, since ICP uses deterministic computing to obtain cryptographic random values.

At the low level, ICP uses a Verifiable Random Function that is exposed by the management canister and is used by the Motoko `Random` module. In each execution round, the randomness function is evaluated with number of the current round used as input in order to produce a fresh set of random bytes.

To use randomness, certain guidelines must be followed:

- The randomness source must only be obtainable asynchronously in chunks of 256 bits that are 32-byte sized `Blobs`.

- Bets must be closed before the randomness source is requested. This implies that the same randomness source cannot be used for a new round of bets without losing its cryptographic guarantee.

The Random module features a class called `Finite` and a `*From` method. These carry the risk of carrying over the state from a previous round, but they are provided for performance and convenience reasons. They should be used carefully.


## `Random` module example

To demonstrate randomness, consider the following example that shuffles a deck of cards then returns the cards in their shuffled order. The code is annotated with additional information:

```motoko
// Import the necessary modules, including the Random module:
import Random = "mo:base/Random";
import Char = "mo:base/Char";
import Error = "mo:base/Error";

// Define an actor

actor {

  // Define a stable variable that contains each card as a unicode character:
  stable var deck : ?[var Char] = ?[var
    '🂡','🂢','🂣','🂤','🂥','🂦','🂧','🂨','🂩','🂪','🂫','🂬','🂭','🂮',
    '🂱','🂲','🂳','🂴','🂵','🂶','🂷','🂸','🂹','🂺','🂻','🂼','🂽','🂾',
    '🃁','🃂','🃃','🃄','🃅','🃆','🃇','🃈','🃉','🃊','🃋','🃌','🃍','🃎',
    '🃑','🃒','🃓','🃔','🃕','🃖','🃗','🃘','🃙','🃚','🃛','🃜','🃝','🃞',
    '🃏'
  ];

  func bit(b : Bool) : Nat {
    if (b) 1 else 0;
  };

  // Use a finite source of randomness defined as `f`.
  // Return an optional random number between [0..`max`) using rejection sampling.
  // A return value of `null` indicates that `f` is exhausted and should be replaced.
  func chooseMax(f : Random.Finite, max : Nat) : ? Nat {
    assert max > 0;
    do ? {
      var n = max - 1 : Nat;
      var k = 0;
      while (n != 0) {
        k *= 2;
        k += bit(f.coin()!);
        n /= 2;
      };
      if (k < max) k else chooseMax(f, max)!;
    };
  };

  // Define a function to shuffle the cards using `Random.Finite`.
  public func shuffle() : async () {
    let ?cards = deck else throw Error.reject("shuffle in progress");
    deck := null;
    var f = Random.Finite(await Random.blob());
    var i : Nat = cards.size() - 1;
    while (i > 0) {
      switch (chooseMax(f, i + 1)) {
        case (?j) {
          let temp = cards[i];
          cards[i] := cards[j];
          cards[j] := temp;
          i -= 1;
        };
        case null { // need more entropy
          f := Random.Finite(await Random.blob());
        }
      }
    };
    deck := ?cards;
  };

  // Define a function to display the randomly shuffled cards.
  public query func show() : async Text {
    let ?cards = deck else throw Error.reject("shuffle in progress");
    var t = "";
    for (card in cards.vals()) {
       t #= Char.toText(card);
    };
    return t;
  }

};
```

View this example on the [Motoko Playground](https://play.motoko.org/?tag=2675232834) or on [GitHub](https://github.com/crusso/card-shuffle/blob/main/src/cards_backend/main.mo).

:::tip

The above solution directly uses the finite blob of 256-random bits returned by the management canister. Class `Random.Finite` uses this finite supply of bits to generate at most 256 coin flips, returning `null` when no more flips are possible.

When its current supply of bits is exhausted, the code asynchronously requests another 256-bit blob to continue the shuffle. A more efficient, and equally robust approach would be to use the first 256-bit blob as a seed to a sequential pseudo random number generator, generating an infinite, lazy stream of bits, and then complete the shuffle with a single round of communication.

:::

## Calling the management canister's `raw_rand` method

Alternatively, you can use randomness by calling the management canister's `raw_rand` endpoint:

```motoko
actor {
  let SubnetManager : actor {
    raw_rand() : async Blob;
  } = actor "aaaaa-aa";

  public func random_bytes() : async Blob {
    let bytes = await SubnetManager.raw_rand();
    return bytes;
  };
};
```

## Resources

- [Onchain randomness](https://internetcomputer.org/docs/current/developer-docs/smart-contracts/advanced-features/randomness)

- [Random base library documentation](../base/Random.md)

<img src="https://github.com/user-attachments/assets/844ca364-4d71-42b3-aaec-4a6c3509ee2e" alt="Logo" width="150" height="150" />