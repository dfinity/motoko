/// Collection of pseudo-random number generators
///
/// The algorithms deliver deterministic statistical randomness,
/// not cryptographic randomness.
///
/// Algorithm 1: 128-bit Seiran PRNG
/// See: https://github.com/andanteyk/prng-seiran
///
/// Algorithm 2: SFC64 and SFC32 (Chris Doty-Humphrey’s Small Fast Chaotic PRNG)
/// See: https://numpy.org/doc/stable/reference/random/bit_generators/sfc64.html
///
/// Copyright: 2023 MR Research AG
/// Main author: react0r-com
/// Contributors: Timo Hanke (timohanke)
import Nat "../Nat";

module {
  /// Constructs an SFC 64-bit generator.
  /// The recommended constructor arguments are: 24, 11, 3.
  ///
  /// Example:
  /// ```motoko
  /// import PRNG "mo:core/internal/PRNG";
  ///
  /// let rng = PRNG.SFC64(24, 11, 3);
  /// ```
  /// For convenience, the function `SFC64a()` returns a generator constructed
  /// with the recommended parameter set (24, 11, 3).
  public class SFC64(p : Nat64, q : Nat64, r : Nat64) {
    // state
    var a : Nat64 = 0;
    var b : Nat64 = 0;
    var c : Nat64 = 0;
    var d : Nat64 = 0;

    /// Initializes the PRNG state with a particular seed
    ///
    /// Example:
    /// ```motoko
    public func init(seed : Nat64) = init3(seed, seed, seed);

    /// Initializes the PRNG state with a hardcoded seed.
    /// No argument is required.
    ///
    /// Example:
    public func initPre() = init(0xcafef00dbeef5eed);

    /// Initializes the PRNG state with three state variables
    ///
    /// Example:
    public func init3(seed1 : Nat64, seed2 : Nat64, seed3 : Nat64) {
      a := seed1;
      b := seed2;
      c := seed3;
      d := 1;

      for (_ in Nat.range(0, 11)) ignore next()
    };

    /// Returns one output and advances the PRNG's state
    ///
    /// Example:
    public func next() : Nat64 {
      let tmp = a +% b +% d;
      a := b ^ (b >> q);
      b := c +% (c << r);
      c := (c <<> p) +% tmp;
      d +%= 1;
      tmp
    }
  };

  /// SFC64a is the same as numpy.
  /// See: [sfc64_next()](https:///github.com/numpy/numpy/blob/b6d372c25fab5033b828dd9de551eb0b7fa55800/numpy/random/src/sfc64/sfc64.h#L28)
  public func sfc64a() : SFC64 { SFC64(24, 11, 3) }
}
