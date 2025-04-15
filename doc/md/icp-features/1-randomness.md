---
sidebar_position: 1
---

# Randomness

[Randomness](https://internetcomputer.org/docs/building-apps/network-features/randomness) is used for generating unique identifiers, ensuring fairness in games, cryptographic protocols, and much more. On ICP, all computations, including [randomness](https://internetcomputer.org/docs/building-apps/network-features/randomness), must be **verifiable and reproducible** across the network's nodes.

The network provides a **verifiable random function (VRF)** through the [management canister](https://internetcomputer.org/docs/references/system-canisters/management-canister) that produces random values that are **unpredictable yet verifiable**, ensuring fairness and security while maintaining network consensus. It guarantees cryptographic security, making it suitable for use cases such as cryptographic key generation.

The VRF generates 256-bit random `Blob`s in each execution round. Your canister can request one of these random `Blob`s via the management canister's `raw_rand` method.

<!--PRNG to point to correct Base library reference later -->

Motoko provides multiple options for incorporating randomness into your code, each suited for different scenarios:

1. Calling the `raw_rand` method of the management canister directly.

2. Using the [`Random`](https://internetcomputer.org/docs/motoko/base/Random) module in the base library offers an interface to work with random values derived from `raw_rand`, including finite sources of randomness for efficient computations.

3. For applications that do not require strong cryptographic guarantees, **time-based randomness** can be generated using the [`fuzz` package](https://mops.one/fuzz), which provides a lightweight alternative without relying on network calls.

4. For generating **unique identifiers**, the [`idempotency-keys` package](https://mops.one/idempotency-keys) offers UUID v4 generation, ensuring globally unique values for transactions and distributed systems.

5. For deterministic statistical randomness, the [`PRNG` module](https://dfinity.github.io/new-motoko-base/internal/PRNG.html) provides high-quality pseudo-random number generators that are fast and suitable for simulations and games.

The right method for your application depends on its security, performance, and reproducibility requirements.

| Method              | Functionality     | Security level      | Example use cases        | Key features |
|--------------------|-------------------|---------------------|------------------|--------------|
| `raw_rand` function    | Returns 32 bytes of cryptographic randomness from ICP’s VRF.   | Strong cryptographic guarantees, ensures unpredictability.  | Secure key generation, fairness-compliant applications, unpredictable randomness. | Directly retrieves randomness from the network’s consensus layer, 32-byte (256-bit) `Blob`s, asynchronous, returns fresh entropy each call. |
| [`Random` module](https://internetcomputer.org/docs/motoko/base/Random)   | High-level wrapper for `raw_rand`, providing finite random pools.      | Uses `raw_rand`, but requires careful handling to avoid entropy reuse.  | Random number generation, shuffling, simulations. | Simplifies number generation, includes finite entropy pools, requires fresh `raw_rand` calls when exhausted. |
| [`fuzz` package](https://mops.one/fuzz)     | Pseudo-random generator that can be seeded with time, `Blob`s, or custom functions. | Security depends on the seed. | Fuzz testing, procedural generation, simulations, dynamic randomness. | Default seed is `Time.now` (low security), can be initialized with `raw_rand` for high security, supports custom generators. |
| [`idempotency-keys` package](https://mops.one/idempotency-keys)  | Generates UUID v4 from a 16-byte random seed. | Security depends on the provided entropy.   | Unique transaction IDs, idempotency, database keys.   | Produces RFC4122-compliant UUIDs, requires secure entropy source, simple API `UUID.generateV4(seed)`. |
| [`PRNG` module](https://dfinity.github.io/new-motoko-base/internal/PRNG.html)  | Collection of deterministic pseudo-random number generators.  | Statistical randomness, not cryptographic randomness. | Games, simulations, procedural generation, testing. | Fast algorithms (SFC64/32, Seiran PRNG), deterministic output for a given seed, no network calls required. |

:::info
Before using the [`fuzz`](https://mops.one/fuzz) or [`idempotency-keys`](https://mops.one/idempotency-keys) packages, ensure that [Mops](https://mops.one/) is installed and initialized in your Motoko project.
:::

## `raw_rand`

The `raw_rand` function is a system API provided by the [ICP management canister](https://internetcomputer.org/docs/references/system-canisters/management-canister) for requesting cryptographic randomness derived from the network’s verifiable random function. `raw_rand` generates fresh entropy in every execution round, making it suitable for applications requiring high security such as key generation or lotteries where fairness is a legal requirement.

Since `raw_rand` operates asynchronously, canisters must await its response before using the generated bytes. Each call returns a 32-byte (256-bit) random `Blob`.

```motoko no-repl
actor {
  let SubnetManager : actor {
    raw_rand() : async Blob;
  } = actor "aaaaa-aa";

  public func random_bytes() : async Blob {
    await SubnetManager.raw_rand();
  };
}
```

## `Random`

The [`Random` module](https://internetcomputer.org/docs/motoko/base/Random) provides an interface that wraps the `raw_rand` function. Since `raw_rand` returns raw bytes, the `Random` module simplifies working with this entropy by offering structured methods for consuming randomness efficiently. The module includes `Random.blob()` for fetching fresh 32-byte entropy and `Random.Finite`, which provides a finite source of randomness that can be used until exhausted. When entropy runs out, a new random `Blob` must be fetched asynchronously.

Below is an example demonstrating how to generate a random boolean using `Random.Finite`.

```motoko no-repl
import Random "mo:base/Random";

actor {
  public func random_boolean() : async ?Bool {
    let entropy = await Random.blob();
    let finite = Random.Finite(entropy);
    // Consumes 1 byte of entropy
    finite.coin();
  };
}
```

## `PRNG`

The [`PRNG` module](https://mops.one/prng) provides a collection of pseudo-random number generators that deliver deterministic statistical randomness. Unlike cryptographic randomness, these algorithms are designed for performance and are suitable for games, simulations, and other applications where cryptographic security is not required.

The package includes two main algorithms:

1. 128-bit Seiran PRNG
2. SFC64 and SFC32 (Small Fast Chaotic PRNG)

The following example demonstrates using the SFC64 generator with recommended parameters:

```motoko no-repl
import Prng "mo:prng";

actor {
  let rng = Prng.SFC64(24, 11, 3);
  
  public func initialize() {
    rng.init(12345); // Seed with a specific value
    // Alternatively: rng.initPre(); // Use hardcoded seed
  };
  
  public query func random_number() : async Nat64 {
    rng.next();
  };
}
```

For convenience, the package provides a pre-configured SFC64 generator:

```motoko no-repl
import Prng "mo:prng";

actor {
  let rng = Prng.SFC64a(); // Same as SFC64(24, 11, 3)
  
  public func initialize_with_multiple_seeds() {
    rng.init3(123, 456, 789); // Initialize with three seeds
  };
  
  public query func random_number() : async Nat64 {
    rng.next();
  };
}
```

## `fuzz`

The [`fuzz` package](https://mops.one/fuzz) is a random data generator designed primarily for testing, but it can also be used for generating random account IDs, unique values, and randomized inputs. It supports various data types, including numbers, text, arrays, `Blob`s, and principals. The randomness source is customizable, allowing initialization with a time-based seed, a fixed seed for reproducibility, a random `Blob` for stronger entropy, or a custom generator function. By default, `fuzz` uses `Time.now()` as a seed, providing immediate access to pseudo-random values without external dependencies.

The following example demonstrates initialization with the default seed and generating a random `Nat`.

```motoko no-repl
import Fuzz "mo:fuzz";

  let fuzz = Fuzz.Fuzz();

  public query func random_nat() : async Nat {
    fuzz.nat.random();
  };
```

## `idempotency-keys`

The [`idempotency-keys` package](https://mops.one/idempotency-keys) provides a method for generating universally unique identifiers (UUID) version 4 (v4), ensuring globally unique values suitable for transaction tracking and request deduplication. It takes a 16-byte random seed and formats it according to the UUID v4 specification. The security of the generated UUIDs depends on the entropy source used for the seed, with `raw_rand` being the recommended option for ensuring cryptographic uniqueness. This makes it useful for idempotent API requests, database keys, and other scenarios requiring unique identifiers.

The following example demonstrates generating a UUID v4 using `idempotency-keys`.

```motoko no-repl
import UUID "mo:idempotency-keys/UUID";
import Random "mo:base/Random";

actor {
  public func generate_uuid() : async Text {
    let seed = await Random.blob();
    UUID.v4(seed);
  };
}
```

## Resources

- [`raw_rand`](https://internetcomputer.org/docs/references/ic-interface-spec#ic-raw_rand)
- [`Random`](https://internetcomputer.org/docs/motoko/base/Random)
- [`PRNG`](https://dfinity.github.io/new-motoko-base/internal/PRNG.html)
- [`fuzz`](https://mops.one/fuzz)
- [`idempotency-keys`](https://mops.one/idempotency-keys)
