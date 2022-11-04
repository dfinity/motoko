/// General utilities
///
/// This prelude file proposes standard library features that _may_
/// belong in the _language_ (compiler-internal) prelude sometime, after
/// some further experience and discussion.  Until then, they live here.

import Debug "Debug";

module {

  /// Not yet implemented
  ///
  /// Mark incomplete code with the `nyi` and `xxx` functions.
  ///
  /// Each have calls are well-typed in all typing contexts, which
  /// trap in all execution contexts.
  public func nyi() : None {
    Debug.trap("Prelude.nyi()");
  };

  public func xxx() : None {
    Debug.trap("Prelude.xxx()");
  };

  /// Mark unreachable code with the `unreachable` function.
  ///
  /// Calls are well-typed in all typing contexts, and they
  /// trap in all execution contexts.
  public func unreachable() : None {
    Debug.trap("Prelude.unreachable()")
  };

}
