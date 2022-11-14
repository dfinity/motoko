/// Debugging aids

import Prim "mo:⛔";
module {
  /// This prints a text (which typically does not include a final newline) to the
  /// debug output. Where this debug output is stored and shown depends on the
  /// environment the program runs in.
  public let print : Text -> () = Prim.debugPrint;
}
