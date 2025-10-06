/// Managing cycles within actors in the Internet Computer Protocol (ICP).
///
/// The usage of the Internet Computer is measured, and paid for, in _cycles_.
/// This library provides imperative operations for observing cycles, transferring cycles, and
/// observing refunds of cycles.
///
/// **NOTE:** Since cycles measure computational resources, the value of  `balance()` can change from one call to the next.
///
/// Cycles can be transferred from the current actor to another actor with the evaluation of certain forms of expression.
/// In particular, the expression must be a call to a shared function, a call to a local function with an `async` return type, or a simple `async` expression.
/// To attach an amount of cycles to an expression `<exp>`, simply prefix the expression with `(with cycles = <amount>)`, that is, `(with cycles = <amount>) <exp>`.
///
/// **NOTE:** Attaching cycles will trap if the amount specified exceeds `2 ** 128` cycles.
///
/// Upon the call, but not before, the amount of cycles is deducted from `balance()`.
/// If this total exceeds `balance()`, the caller traps, aborting the call without consuming the cycles.
/// Note that attaching cycles to a call to a local function call or `async` expression just transfers cycles from the current actor to itself.
///
/// Example for use on the ICP:
/// ```motoko no-repl
/// import Cycles "mo:core/Cycles";
///
/// persistent actor {
///   public func main() : async () {
///     let initialBalance = Cycles.balance();
///     await (with cycles = 15_000_000) operation(); // accepts 10_000_000 cycles
///     assert Cycles.refunded() == 5_000_000;
///     assert Cycles.balance() < initialBalance; // decreased by around 10_000_000
///   };
///
///   func operation() : async () {
///     let initialBalance = Cycles.balance();
///     let initialAvailable = Cycles.available();
///     let obtained = Cycles.accept<system>(10_000_000);
///     assert obtained == 10_000_000;
///     assert Cycles.balance() == initialBalance + 10_000_000;
///     assert Cycles.available() == initialAvailable - 10_000_000;
///   }
/// }
/// ```
import Prim "mo:⛔";
module {

  /// Returns the actor's current balance of cycles as `amount`.
  ///
  /// Example for use on the ICP:
  /// ```motoko no-repl
  /// import Cycles "mo:core/Cycles";
  ///
  /// persistent actor {
  ///   public func main() : async() {
  ///     let balance = Cycles.balance();
  ///     assert balance > 0;
  ///   }
  /// }
  /// ```
  public let balance : () -> (amount : Nat) = Prim.cyclesBalance;

  /// Returns the currently available `amount` of cycles.
  /// The amount available is the amount received in the current call,
  /// minus the cumulative amount `accept`ed by this call.
  /// On exit from the current shared function or async expression via `return` or `throw`,
  /// any remaining available amount is automatically refunded to the caller/context.
  ///
  /// Example for use on the ICP:
  /// ```motoko no-repl
  /// import Cycles "mo:core/Cycles";
  ///
  /// persistent actor {
  ///   public func main() : async() {
  ///     let available = Cycles.available();
  ///     assert available >= 0;
  ///   }
  /// }
  /// ```
  public let available : () -> (amount : Nat) = Prim.cyclesAvailable;

  /// Transfers up to `amount` from `available()` to `balance()`.
  /// Returns the amount actually transferred, which may be less than
  /// requested, for example, if less is available, or if canister balance limits are reached.
  ///
  /// Example for use on the ICP (for simplicity, only transferring cycles to itself):
  /// ```motoko no-repl
  /// import Cycles "mo:core/Cycles";
  ///
  /// persistent actor {
  ///   public func main() : async() {
  ///     await (with cycles = 15_000_000) operation(); // accepts 10_000_000 cycles
  ///   };
  ///
  ///   func operation() : async() {
  ///     let obtained = Cycles.accept<system>(10_000_000);
  ///     assert obtained == 10_000_000;
  ///   }
  /// }
  /// ```
  public let accept : <system>(amount : Nat) -> (accepted : Nat) = Prim.cyclesAccept;

  /// Reports `amount` of cycles refunded in the last `await` of the current
  /// context, or zero if no await has occurred yet.
  /// Calling `refunded()` is solely informational and does not affect `balance()`.
  /// Instead, refunds are automatically added to the current balance,
  /// whether or not `refunded` is used to observe them.
  ///
  /// Example for use on the ICP (for simplicity, only transferring cycles to itself):
  /// ```motoko no-repl
  /// import Cycles "mo:core/Cycles";
  ///
  /// persistent actor {
  ///   func operation() : async() {
  ///     ignore Cycles.accept<system>(10_000_000);
  ///   };
  ///
  ///   public func main() : async() {
  ///     await (with cycles = 15_000_000) operation(); // accepts 10_000_000 cycles
  ///     assert Cycles.refunded() == 5_000_000;
  ///   }
  /// }
  /// ```
  public let refunded : () -> (amount : Nat) = Prim.cyclesRefunded;

  /// Attempts to burn `amount` of cycles, deducting `burned` from the canister's
  /// cycle balance. The burned cycles are irrevocably lost and not available to any
  /// other principal either.
  ///
  /// Example for use on the IC:
  /// ```motoko no-repl
  /// import Cycles "mo:core/Cycles";
  ///
  /// persistent actor {
  ///   public func main() : async() {
  ///     let burnt = Cycles.burn<system>(10_000_000);
  ///     assert burnt == 10_000_000;
  ///   }
  /// }
  /// ```
  public let burn : <system>(amount : Nat) -> (burned : Nat) = Prim.cyclesBurn;

}
