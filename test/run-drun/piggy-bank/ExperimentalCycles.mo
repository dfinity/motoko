/// Managing cycles
///
/// Usage of the Internet Computer is measured, and paid for, in _cycles_.
/// This library provides imperative operations for observing cycles, transferring cycles and
/// observing refunds of cycles.
///
/// **WARNING:** This low-level API is **experimental** and likely to change or even disappear.
/// Dedicated syntactic support for manipulating cycles may be added to the language in future, obsoleting this library.
///
/// **NOTE:** Since cycles measure computational resources, the value of
/// `balance()` can change from one call to the next.

import Prim "mo:⛔";
module {

  /// Returns the actor's current balance of cycles as `amount`.
  public func balance() : (amount : Nat) {
    Prim.nat64ToNat(Prim.cyclesBalance())
  };

  /// Returns the currently available `amount` of cycles.
  /// The amount available is the amount received in the current call,
  /// minus the cumulative amount `accept`ed by this call.
  /// On exit from the current shared function or async expression via `return` or `throw`
  /// any remaining available amount is automatically
  /// refunded to the caller/context.
  public func available() : (amount : Nat) {
    Prim.nat64ToNat(Prim.cyclesAvailable())
  };

  /// Transfers up to `amount` from `available()` to `balance()`.
  /// Returns the amount actually transferred, which may be less than
  /// requested, for example, if less is available, or if canister balance limits are reached.
  public func accept(amount : Nat) : (accepted : Nat) {
    Prim.nat64ToNat(Prim.cyclesAccept(Prim.natToNat64(amount)));
  };

  /// Indicates additional `amount` of cycles to be transferred in
  /// the next call, that is, evaluation of a shared function call or
  /// async expression.
  /// Upon the call, but not before, the total amount of cycles ``add``ed since
  /// the last call is deducted from `balance()`.
  /// If this total exceeds `balance()`, the caller traps, aborting the call.
  ///
  /// **Note**: the implicit register of added amounts is reset to zero on entry to
  /// a shared function and after each shared function call or resume from an await.
  public func add(amount : Nat) : () {
    Prim.cyclesAdd(Prim.natToNat64(amount))
  };

  /// Reports `amount` of cycles refunded in the last `await` of the current
  /// context, or zero if no await has occurred yet.
  /// Calling `refunded()` is solely informational and does not affect `balance()`.
  /// Instead, refunds are automatically added to the current balance,
  /// whether or not `refunded` is used to observe them.
  public func refunded() : (amount : Nat) {
    Prim.nat64ToNat(Prim.cyclesRefunded())
  };

}
