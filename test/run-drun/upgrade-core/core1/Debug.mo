/// Utility functions for debugging.
///
/// Import from the core package to use this module.
/// ```motoko name=import
/// import Debug "mo:core/Debug";
/// ```

import Prim "mo:⛔";
import Runtime "Runtime";

module {

  /// Prints `text` to output stream.
  ///
  /// NOTE: When running on an ICP network, all output is written to the [canister log](https://internetcomputer.org/docs/current/developer-docs/smart-contracts/maintain/logs) with the exclusion of any output
  /// produced during the execution of non-replicated queries and composite queries.
  /// In other environments, like the interpreter and stand-alone wasm engines, the output is written to standard out.
  ///
  /// ```motoko include=import
  /// Debug.print "Hello New World!";
  /// Debug.print(debug_show(4)) // Often used with `debug_show` to convert values to Text
  /// ```
  public func print(text : Text) {
    Prim.debugPrint(text)
  };

  /// Mark incomplete code with the `todo()` function.
  ///
  /// Each have calls are well-typed in all typing contexts, which
  /// trap in all execution contexts.
  ///
  /// ```motoko include=import
  /// func doSomethingComplex() {
  ///   Debug.todo()
  /// };
  /// ```
  public func todo() : None {
    Runtime.trap("Debug.todo()")
  };

}
