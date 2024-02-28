/// Managing cycles within actors on the Internet Computer (IC).
///
/// The usage of the Internet Computer is measured, and paid for, in _cycles_.
/// This library provides imperative operations for observing cycles, transferring cycles, and
/// observing refunds of cycles.
///
/// **WARNING:** This low-level API is **experimental** and likely to change or even disappear.
/// Dedicated syntactic support for manipulating cycles may be added to the language in future, obsoleting this library.
///
/// **NOTE:** Since cycles measure computational resources, the value of  `balance()` can change from one call to the next.
///
/// Example for use on IC:
/// ```motoko no-repl
/// import Cycles "mo:base/ExperimentalCycles";
/// import Debug "mo:base/Debug";
///
/// actor {
///   public func main() : async() {
///     Debug.print("Main balance: " # debug_show(Cycles.balance()));
///     Cycles.add<system>(15_000_000);
///     await operation(); // accepts 10_000_000 cycles
///     Debug.print("Main refunded: " # debug_show(Cycles.refunded())); // 5_000_000
///     Debug.print("Main balance: " # debug_show(Cycles.balance())); // decreased by around 10_000_000
///   };
///
///   func operation() : async() {
///     Debug.print("Operation balance: " # debug_show(Cycles.balance()));
///     Debug.print("Operation available: " # debug_show(Cycles.available()));
///     let obtained = Cycles.accept<system>(10_000_000);
///     Debug.print("Operation obtained: " # debug_show(obtained)); // => 10_000_000
///     Debug.print("Operation balance: " # debug_show(Cycles.balance())); // increased by 10_000_000
///     Debug.print("Operation available: " # debug_show(Cycles.available())); // decreased by 10_000_000
///   }
/// }
/// ```
import Prim "mo:⛔";
module {

  /// Returns the actor's current balance of cycles as `amount`.
  ///
  /// Example for use on the IC:
  /// ```motoko no-repl
  /// import Cycles "mo:base/ExperimentalCycles";
  /// import Debug "mo:base/Debug";
  ///
  /// actor {
  ///   public func main() : async() {
  ///     let balance = Cycles.balance();
  ///     Debug.print("Balance: " # debug_show(balance));
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
  /// Example for use on the IC:
  /// ```motoko no-repl
  /// import Cycles "mo:base/ExperimentalCycles";
  /// import Debug "mo:base/Debug";
  ///
  /// actor {
  ///   public func main() : async() {
  ///     let available = Cycles.available();
  ///     Debug.print("Available: " # debug_show(available));
  ///   }
  /// }
  /// ```
  public let available : () -> (amount : Nat) = Prim.cyclesAvailable;

  /// Transfers up to `amount` from `available()` to `balance()`.
  /// Returns the amount actually transferred, which may be less than
  /// requested, for example, if less is available, or if canister balance limits are reached.
  ///
  /// Example for use on the IC (for simplicity, only transferring cycles to itself):
  /// ```motoko no-repl
  /// import Cycles "mo:base/ExperimentalCycles";
  /// import Debug "mo:base/Debug";
  ///
  /// actor {
  ///   public func main() : async() {
  ///     Cycles.add<system>(15_000_000);
  ///     await operation(); // accepts 10_000_000 cycles
  ///   };
  ///
  ///   func operation() : async() {
  ///     let obtained = Cycles.accept<system>(10_000_000);
  ///     Debug.print("Obtained: " # debug_show(obtained)); // => 10_000_000
  ///   }
  /// }
  /// ```
  public let accept : <system>(amount : Nat) -> (accepted : Nat) = Prim.cyclesAccept;

  /// Indicates additional `amount` of cycles to be transferred in
  /// the next call, that is, evaluation of a shared function call or
  /// async expression.
  /// Traps if the current total would exceed `2 ** 128` cycles.
  /// Upon the call, but not before, the total amount of cycles ``add``ed since
  /// the last call is deducted from `balance()`.
  /// If this total exceeds `balance()`, the caller traps, aborting the call.
  ///
  /// **Note**: The implicit register of added amounts is reset to zero on entry to
  /// a shared function and after each shared function call or resume from an await.
  ///
  /// Example for use on the IC (for simplicity, only transferring cycles to itself):
  /// ```motoko no-repl
  /// import Cycles "mo:base/ExperimentalCycles";
  ///
  /// actor {
  ///   func operation() : async() {
  ///     ignore Cycles.accept<system>(10_000_000);
  ///   };
  ///
  ///   public func main() : async() {
  ///     Cycles.add<system>(15_000_000);
  ///     await operation();
  ///   }
  /// }
  /// ```
  public let add : <system>(amount : Nat) -> () = Prim.cyclesAdd;

  /// Reports `amount` of cycles refunded in the last `await` of the current
  /// context, or zero if no await has occurred yet.
  /// Calling `refunded()` is solely informational and does not affect `balance()`.
  /// Instead, refunds are automatically added to the current balance,
  /// whether or not `refunded` is used to observe them.
  ///
  /// Example for use on the IC (for simplicity, only transferring cycles to itself):
  /// ```motoko no-repl
  /// import Cycles "mo:base/ExperimentalCycles";
  /// import Debug "mo:base/Debug";
  ///
  /// actor {
  ///   func operation() : async() {
  ///     ignore Cycles.accept<system>(10_000_000);
  ///   };
  ///
  ///   public func main() : async() {
  ///     Cycles.add<system>(15_000_000);
  ///     await operation(); // accepts 10_000_000 cycles
  ///     Debug.print("Refunded: " # debug_show(Cycles.refunded())); // 5_000_000
  ///   }
  /// }
  /// ```
  public let refunded : () -> (amount : Nat) = Prim.cyclesRefunded;

}
