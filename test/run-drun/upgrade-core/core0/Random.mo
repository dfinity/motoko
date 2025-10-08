/// Random number generation.
///
/// Import from the core package to use this module.
/// ```motoko name=import
/// import Random "mo:core/Random";
/// ```

import Array "Array";
import VarArray "VarArray";
import Nat8 "Nat8";
import Nat64 "Nat64";
import Int "Int";
import Nat "Nat";
import Blob "Blob";
import Runtime "Runtime";

module {

  public type State = {
    var bytes : [Nat8];
    var index : Nat;
    var bits : Nat8;
    var bitMask : Nat8
  };

  public type SeedState = {
    random : State;
    prng : PRNG.State
  };

  let rawRand = (actor "aaaaa-aa" : actor { raw_rand : () -> async Blob }).raw_rand;

  public let blob : shared () -> async Blob = rawRand;

  /// Initializes a random number generator state. This is used
  /// to create a `Random` or `AsyncRandom` instance with a specific state.
  /// The state is empty, but it can be reused after upgrading the canister.
  ///
  /// Example:
  /// ```motoko
  /// import Random "mo:core/Random";
  ///
  /// persistent actor {
  ///   let state = Random.emptyState();
  ///   transient let random = Random.cryptoFromState(state);
  ///
  ///   public func main() : async () {
  ///     let coin = await* random.bool(); // true or false
  ///   }
  /// }
  /// ```
  public func emptyState() : State = {
    var bytes = [];
    var index = 0;
    var bits = 0x00;
    var bitMask = 0x00
  };

  /// Initializes a pseudo-random number generator state with a 64-bit seed.
  /// This is used to create a `Random` instance with a specific seed.
  /// The seed is used to initialize the PRNG state.
  ///
  /// Example:
  /// ```motoko
  /// import Random "mo:core/Random";
  ///
  /// persistent actor {
  ///   let state = Random.seedState(123);
  ///   transient let random = Random.seedFromState(state);
  ///
  ///   public func main() : async () {
  ///     let coin = random.bool(); // true or false
  ///   }
  /// }
  /// ```
  public func seedState(seed : Nat64) : SeedState = {
    random = emptyState();
    prng = PRNG.init(seed)
  };

  /// Creates a pseudo-random number generator from a 64-bit seed.
  /// The seed is used to initialize the PRNG state.
  /// This is suitable for simulations and testing, but not for cryptographic purposes.
  ///
  /// Example:
  /// ```motoko include=import
  /// let random = Random.seed(123);
  /// let coin = random.bool(); // true or false
  /// ```
  public func seed(seed : Nat64) : Random {
    seedFromState(seedState(seed))
  };

  /// Creates a pseudo-random number generator with the given state.
  /// This provides statistical randomness suitable for simulations and testing,
  /// but should not be used for cryptographic purposes.
  ///
  /// Example:
  /// ```motoko
  /// import Random "mo:core/Random";
  ///
  /// persistent actor {
  ///   let state = Random.seedState(123);
  ///   transient let random = Random.seedFromState(state);
  ///
  ///   public func main() : async () {
  ///     let coin = random.bool(); // true or false
  ///   }
  /// }
  /// ```
  public func seedFromState(state : SeedState) : Random {
    Random(
      state.random,
      func() : Blob {
        // Generate 8 bytes directly from a single 64-bit number
        let n = PRNG.next(state.prng);
        // TODO: optimize using Array.tabulate or even better: a new primitive
        let bytes = VarArray.repeat<Nat8>(0, 8);
        bytes[0] := Nat8.fromNat(Nat64.toNat(n & 0xFF));
        bytes[1] := Nat8.fromNat(Nat64.toNat((n >> 8) & 0xFF));
        bytes[2] := Nat8.fromNat(Nat64.toNat((n >> 16) & 0xFF));
        bytes[3] := Nat8.fromNat(Nat64.toNat((n >> 24) & 0xFF));
        bytes[4] := Nat8.fromNat(Nat64.toNat((n >> 32) & 0xFF));
        bytes[5] := Nat8.fromNat(Nat64.toNat((n >> 40) & 0xFF));
        bytes[6] := Nat8.fromNat(Nat64.toNat((n >> 48) & 0xFF));
        bytes[7] := Nat8.fromNat(Nat64.toNat((n >> 56) & 0xFF));
        Blob.fromArray(Array.fromVarArray(bytes))
      }
    )
  };

  /// Initializes a cryptographic random number generator
  /// using entropy from the ICP management canister.
  ///
  /// Example:
  /// ```motoko
  /// import Random "mo:core/Random";
  ///
  /// persistent actor {
  ///   transient let random = Random.crypto();
  ///
  ///   public func main() : async () {
  ///     let coin = await* random.bool(); // true or false
  ///   }
  /// }
  /// ```
  public func crypto() : AsyncRandom {
    cryptoFromState(emptyState())
  };

  /// Creates a random number generator suitable for cryptography
  /// using entropy from the ICP management canister. Initializing
  /// from a state makes it possible to reuse entropy after
  /// upgrading the canister.
  ///
  /// Example:
  /// ```motoko
  /// import Random "mo:core/Random";
  ///
  /// persistent actor {
  ///   let state = Random.emptyState();
  ///   transient let random = Random.cryptoFromState(state);
  ///
  ///   func example() : async () {
  ///     let coin = await* random.bool(); // true or false
  ///   }
  /// }
  /// ```
  public func cryptoFromState(state : State) : AsyncRandom {
    AsyncRandom(state, func() : async* Blob { await rawRand() })
  };

  public class Random(state : State, generator : () -> Blob) {

    func nextBit() : Bool {
      if (0 : Nat8 == state.bitMask) {
        state.bits := nat8();
        state.bitMask := 0x40;
        0 : Nat8 != state.bits & (0x80 : Nat8)
      } else {
        let m = state.bitMask;
        state.bitMask >>= (1 : Nat8);
        0 : Nat8 != state.bits & m
      }
    };

    /// Random choice between `true` and `false`.
    ///
    /// Example:
    /// ```motoko include=import
    /// let random = Random.seed(42);
    /// let coin = random.bool(); // true or false
    /// ```
    public func bool() : Bool {
      nextBit()
    };

    /// Random `Nat8` value in the range [0, 256).
    ///
    /// Example:
    /// ```motoko include=import
    /// let random = Random.seed(42);
    /// let byte = random.nat8(); // 0 to 255
    /// ```
    public func nat8() : Nat8 {
      if (state.index >= state.bytes.size()) {
        let newBytes = Blob.toArray(generator());
        if (newBytes.size() == 0) {
          Runtime.trap("Random: generator produced empty Blob")
        };
        state.bytes := newBytes;
        state.index := 0
      };
      let byte = state.bytes[state.index];
      state.index += 1;
      byte
    };

    // Helper function which returns a uniformly sampled `Nat64` in the range `[0, max]`.
    // Uses rejection sampling to ensure uniform distribution even when the range
    // doesn't divide evenly into 2^64. This avoids modulo bias that would occur
    // from simply taking the modulo of a random 64-bit number.
    func uniform64(max : Nat64) : Nat64 {
      if (max == 0) {
        return 0
      };
      // if (max == 1) {
      //   return switch (bool()) {
      //     case false 0;
      //     case true 1
      //   }
      // };
      if (max == Nat64.maxValue) {
        return nat64()
      };
      let toExclusive = max + 1;
      // 2^64 - (2^64 % toExclusive) = (2^64-1) - (2^64-1 % toExclusive):
      let cutoff = Nat64.maxValue - (Nat64.maxValue % toExclusive);
      // 2^64 / toExclusive, with toExclusive > 1:
      let multiple = Nat64.fromNat(/* 2^64 */ 0x10000000000000000 / Nat64.toNat(toExclusive));
      loop {
        // Build up a random Nat64 from bytes
        var number = nat64();
        // If number is below cutoff, we can use it
        if (number < cutoff) {
          // Scale down to desired range
          return number / multiple
        };
        // Otherwise reject and try again
      }
    };

    /// Random `Nat64` value in the range [0, 2^64).
    ///
    /// Example:
    /// ```motoko include=import
    /// let random = Random.seed(42);
    /// let number = random.nat64(); // 0 to 18446744073709551615
    /// ```
    public func nat64() : Nat64 {
      (Nat64.fromNat(Nat8.toNat(nat8())) << 56) | (Nat64.fromNat(Nat8.toNat(nat8())) << 48) | (Nat64.fromNat(Nat8.toNat(nat8())) << 40) | (Nat64.fromNat(Nat8.toNat(nat8())) << 32) | (Nat64.fromNat(Nat8.toNat(nat8())) << 24) | (Nat64.fromNat(Nat8.toNat(nat8())) << 16) | (Nat64.fromNat(Nat8.toNat(nat8())) << 8) | Nat64.fromNat(Nat8.toNat(nat8()))
    };

    /// Random `Nat64` value in the range [fromInclusive, toExclusive).
    ///
    /// Example:
    /// ```motoko include=import
    /// let random = Random.seed(42);
    /// let dice = random.nat64Range(1, 7); // 1 to 6
    /// ```
    public func nat64Range(fromInclusive : Nat64, toExclusive : Nat64) : Nat64 {
      if (fromInclusive >= toExclusive) {
        Runtime.trap("Random.nat64Range(): fromInclusive >= toExclusive")
      };
      uniform64(toExclusive - fromInclusive - 1) + fromInclusive
    };

    /// Random `Nat` value in the range [fromInclusive, toExclusive).
    ///
    /// Example:
    /// ```motoko include=import
    /// let random = Random.seed(42);
    /// let index = random.natRange(0, 10); // 0 to 9
    /// ```
    public func natRange(fromInclusive : Nat, toExclusive : Nat) : Nat {
      if (fromInclusive >= toExclusive) {
        Runtime.trap("Random.natRange(): fromInclusive >= toExclusive")
      };
      Nat64.toNat(uniform64(Nat64.fromNat(toExclusive - fromInclusive - 1))) + fromInclusive
    };

    public func intRange(fromInclusive : Int, toExclusive : Int) : Int {
      let range = Nat.fromInt(toExclusive - fromInclusive - 1);
      Nat64.toNat(uniform64(Nat64.fromNat(range))) + fromInclusive
    };

  };

  public class AsyncRandom(state : State, generator : () -> async* Blob) {

    func nextBit() : async* Bool {
      if (0 : Nat8 == state.bitMask) {
        state.bits := await* nat8();
        state.bitMask := 0x40;
        0 : Nat8 != state.bits & (0x80 : Nat8)
      } else {
        let m = state.bitMask;
        state.bitMask >>= (1 : Nat8);
        0 : Nat8 != state.bits & m
      }
    };

    /// Random choice between `true` and `false`.
    public func bool() : async* Bool {
      await* nextBit()
    };

    /// Random `Nat8` value in the range [0, 256).
    public func nat8() : async* Nat8 {
      if (state.index >= state.bytes.size()) {
        let newBytes = Blob.toArray(await* generator());
        if (newBytes.size() == 0) {
          Runtime.trap("AsyncRandom: generator produced empty Blob")
        };
        state.bytes := newBytes;
        state.index := 0
      };
      let byte = state.bytes[state.index];
      state.index += 1;
      byte
    };

    // Helper function which returns a uniformly sampled `Nat64` in the range `[0, max]`.
    // Uses rejection sampling to ensure uniform distribution even when the range
    // doesn't divide evenly into 2^64. This avoids modulo bias that would occur
    // from simply taking the modulo of a random 64-bit number.
    func uniform64(max : Nat64) : async* Nat64 {
      if (max == 0) {
        return 0
      };
      if (max == Nat64.maxValue) {
        return await* nat64()
      };
      let toExclusive = max + 1;
      // 2^64 - (2^64 % toExclusive) = (2^64-1) - (2^64-1 % toExclusive):
      let cutoff = Nat64.maxValue - (Nat64.maxValue % toExclusive);
      // 2^64 / toExclusive, with toExclusive > 1:
      let multiple = Nat64.fromNat(/* 2^64 */ 0x10000000000000000 / Nat64.toNat(toExclusive));
      loop {
        // Build up a random Nat64 from bytes
        var number = await* nat64();
        // If number is below cutoff, we can use it
        if (number < cutoff) {
          // Scale down to desired range
          return number / multiple
        };
        // Otherwise reject and try again
      }
    };

    /// Random `Nat64` value in the range [0, 2^64).
    public func nat64() : async* Nat64 {
      (Nat64.fromNat(Nat8.toNat(await* nat8())) << 56) | (Nat64.fromNat(Nat8.toNat(await* nat8())) << 48) | (Nat64.fromNat(Nat8.toNat(await* nat8())) << 40) | (Nat64.fromNat(Nat8.toNat(await* nat8())) << 32) | (Nat64.fromNat(Nat8.toNat(await* nat8())) << 24) | (Nat64.fromNat(Nat8.toNat(await* nat8())) << 16) | (Nat64.fromNat(Nat8.toNat(await* nat8())) << 8) | Nat64.fromNat(Nat8.toNat(await* nat8()))
    };

    /// Random `Nat64` value in the range [fromInclusive, toExclusive).
    public func nat64Range(fromInclusive : Nat64, toExclusive : Nat64) : async* Nat64 {
      if (fromInclusive >= toExclusive) {
        Runtime.trap("AsyncRandom.nat64Range(): fromInclusive >= toExclusive")
      };
      (await* uniform64(toExclusive - fromInclusive - 1)) + fromInclusive
    };

    /// Random `Nat` value in the range [fromInclusive, toExclusive).
    public func natRange(fromInclusive : Nat, toExclusive : Nat) : async* Nat {
      if (fromInclusive >= toExclusive) {
        Runtime.trap("AsyncRandom.natRange(): fromInclusive >= toExclusive")
      };
      Nat64.toNat(await* uniform64(Nat64.fromNat(toExclusive - fromInclusive - 1))) + fromInclusive
    };

    /// Random `Int` value in the range [fromInclusive, toExclusive).
    public func intRange(fromInclusive : Int, toExclusive : Int) : async* Int {
      let range = Nat.fromInt(toExclusive - fromInclusive - 1);
      Nat64.toNat(await* uniform64(Nat64.fromNat(range))) + fromInclusive
    };

  };

  // Derived from https://github.com/research-ag/prng
  module PRNG {
    let p : Nat64 = 24;
    let q : Nat64 = 11;
    let r : Nat64 = 3;

    public type State = {
      var a : Nat64;
      var b : Nat64;
      var c : Nat64;
      var d : Nat64
    };

    public func init(seed : Nat64) : State {
      init3(seed, seed, seed)
    };

    public func init3(seed1 : Nat64, seed2 : Nat64, seed3 : Nat64) : State {
      let state : State = {
        var a = seed1;
        var b = seed2;
        var c = seed3;
        var d = 1
      };
      for (_ in Nat.range(0, 11)) ignore next(state);
      state
    };

    public func next(state : State) : Nat64 {
      let tmp = state.a +% state.b +% state.d;
      state.a := state.b ^ (state.b >> q);
      state.b := state.c +% (state.c << r);
      state.c := (state.c <<> p) +% tmp;
      state.d +%= 1;
      tmp
    }
  }

}
