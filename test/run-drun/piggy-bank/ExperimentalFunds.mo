/// Funds and units
///
/// Provides imperative operations for observing funds, transferring funds and
/// observing refunds of various units.
///
/// The two currently supported units are `#cycle` and `#icpt`.
/// Usage of the Internet Computer is measured, and paid for, in unit *Cycle*.
/// *ICPT*  is the unit of Internet Computer Tokens.
///
/// **WARNING:** This low-level API is **experimental** and likely to change or even disappear.
/// More units may be added. Moreover, dedicated syntactic support for
/// manipulating funds may be added to the language in future, obsoleting this library.
///
/// **NOTE:** Since unit `#cycle` also measures computation, the value of
/// `balance(#cycle)` generally decreases from one call to the next.

import Prim "mo:prim";
module {

  /// Units for funds: `{#cycle; #icpt}`.
  public type Unit = Prim.Unit;

  /// Returns the actor's current balance of unit `u` as `amount`.
  public func balance(u : Unit) : (amount : Nat) {
    Prim.nat64ToNat(Prim.fundsBalance(u))
  };

  /// Given `u`, returns the currently available `amount` of unit `u`.
  /// The amount available is the amount received in the current call,
  /// minus the cumulative amount `accept`ed by this call.
  /// On exit from the current shared function or async expression via `return` or `throw`
  /// any remaining available amount is automatically
  /// refunded to the caller/context.
  public func available(u : Unit) : (amount : Nat) {
    Prim.nat64ToNat(Prim.fundsAvailable(u))
  };

  /// Transfers `amount` from `available(u)` to `balance(u)`,
  /// Traps if trying to accept more funds than are available.
  public func accept(u : Unit, amount : Nat) : () {
    Prim.fundsAccept(u, Prim.natToNat64(amount))
  };

  /// Indicates additional `amount` of unit `u` to be transferred in
  /// the next call, i.e. evaluation of a shared function call or
  /// async expression.
  /// Upon the call, but not before, the total amount of units `add`ed since
  /// the last call is deducted from `balance(u)`.
  /// If this total exceeds `balance(u)`, the caller traps, aborting the call.
  ///
  /// Note: the implicit, per unit register of added amounts is reset to zero on entry to
  /// a shared function and after each shared function call or resume from an await.
  public func add(u : Unit, amount : Nat) : () {
    Prim.fundsAdd(u, Prim.natToNat64(amount))
  };

  /// Reports `amount` of unit `u` refunded in the last `await` of the current
  /// context, or `0` if no await has occurred yet.
  /// Calling `refunded(u)` is solely informational and does not affect `balance(u)`.
  /// Instead, refunds are automatically added to the current balance,
  /// whether or not `refunded` is used to observe them.
  public func refunded(u: Unit) : (amount : Nat) {
    Prim.nat64ToNat(Prim.fundsRefunded(u))
  };

}
