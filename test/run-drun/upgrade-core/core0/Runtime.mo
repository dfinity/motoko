/// Runtime utilities.
/// These functions were originally part of the `Debug` module.
///
/// ```motoko name=import
/// import Runtime "mo:core/Runtime";
/// ```
import Prim "mo:⛔";

module {

  /// `trap(t)` traps execution with a user-provided diagnostic message.
  ///
  /// The caller of a future whose execution called `trap(t)` will
  /// observe the trap as an `Error` value, thrown at `await`, with code
  /// `#canister_error` and message `m`. Here `m` is a more descriptive `Text`
  /// message derived from the provided `t`. See example for more details.
  ///
  /// NOTE: Other execution environments that cannot handle traps may only
  /// propagate the trap and terminate execution, with or without some
  /// descriptive message.
  ///
  /// ```motoko include=import no-validate
  /// Runtime.trap("An error occurred!");
  /// ```
  public func trap(errorMessage : Text) : None {
    Prim.trap errorMessage
  };

  /// `unreachable()` traps execution when code that should be unreachable is reached.
  ///
  /// This function is useful for marking code paths that should never be executed,
  /// such as after exhaustive pattern matches or unreachable control flow branches.
  /// If execution reaches this function, it indicates a programming error.
  ///
  /// ```motoko include=import no-validate
  /// let number = switch (?5) {
  ///   case (?n) n;
  ///   case null Runtime.unreachable();
  /// };
  /// assert number == 5;
  /// ```
  public func unreachable() : None {
    trap("Runtime.unreachable()")
  };

}
