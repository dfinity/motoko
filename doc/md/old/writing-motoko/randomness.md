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

```motoko file=../examples/CardShuffle.mo
```

View this example on the [Motoko Playground](https://play.motoko.org/?tag=2675232834) or on [GitHub](https://github.com/crusso/card-shuffle/blob/main/src/cards_backend/main.mo).

:::tip

The above solution directly uses the finite blob of 256-random bits returned by the management canister. Class `Random.Finite` uses this finite supply of bits to generate at most 256 coin flips, returning `null` when no more flips are possible.

When its current supply of bits is exhausted, the code asynchronously requests another 256-bit blob to continue the shuffle. A more efficient, and equally robust approach would be to use the first 256-bit blob as a seed to a sequential pseudo random number generator, generating an infinite, lazy stream of bits, and then complete the shuffle with a single round of communication.

:::

## Calling the management canister's `raw_rand` method

Alternatively, you can use randomness by calling the management canister's `raw_rand` endpoint:

```motoko file=../examples/RawRand.mo
```

## Resources

- [Onchain randomness](https://internetcomputer.org/docs/current/developer-docs/smart-contracts/advanced-features/randomness)

- [Random base library documentation](../base/Random.md)

<img src="https://github.com/user-attachments/assets/844ca364-4d71-42b3-aaec-4a6c3509ee2e" alt="Logo" width="150" height="150" />