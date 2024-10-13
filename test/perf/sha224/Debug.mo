/// Utility functions for debugging.
///
/// Import from the base library to use this module.
/// ```motoko name=import
/// import Debug "mo:base/Debug";
/// ```

import Prim "mo:⛔";
module {
  /// Prints `text` to output stream.
  ///
  /// NOTE: The output is placed in the replica log. When running on mainnet,
  /// this function has no effect.
  ///
  /// ```motoko include=import
  /// Debug.print "Hello New World!";
  /// Debug.print(debug_show(4)) // Often used with `debug_show` to convert values to Text
  /// ```
  public func print(text : Text) {
    Prim.debugPrint text
  };

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
  /// ```motoko
  /// import Debug "mo:base/Debug";
  /// import Error "mo:base/Error";
  ///
  /// actor {
  ///   func fail() : async () {
  ///     Debug.trap("user provided error message");
  ///   };
  ///
  ///   public func foo() : async () {
  ///     try {
  ///       await fail();
  ///     } catch e {
  ///       let code = Error.code(e); // evaluates to #canister_error
  ///       let message = Error.message(e); // contains user provided error message
  ///     }
  ///   };
  /// }
  /// ```
  public func trap(errorMessage : Text) : None {
    Prim.trap errorMessage
  }
}
