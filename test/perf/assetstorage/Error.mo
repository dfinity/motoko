/// Error values and inspection.
///
/// The `Error` type is the argument to `throw`, parameter of `catch`.
/// The `Error` type is opaque.

import Prim "mo:⛔";

module {

  /// Error codes (user and system), where module `Prim` defines:
  /// ```motoko
  /// type ErrorCode = {
  ///   // Fatal error.
  ///   #system_fatal;
  ///   // Transient error.
  ///   #system_transient;
  ///   // Response unknown due to missed deadline.
  ///   #system_unknown;
  ///   // Destination invalid.
  ///   #destination_invalid;
  ///   // Explicit reject by canister code.
  ///   #canister_reject;
  ///   // Canister trapped.
  ///   #canister_error;
  ///   // Future error code (with unrecognized numeric code).
  ///   #future : Nat32;
  ///   // Error issuing inter-canister call
  ///   // (indicating destination queue full or freezing threshold crossed).
  ///   #call_error : { err_code :  Nat32 }
  /// };
  /// ```
  public type ErrorCode = Prim.ErrorCode;

  /// Create an error from message `m` with code #canister_reject.
  public let reject : (m : Text) -> Error = Prim.error;

  /// Returns the code of an error `e`.
  public let code : (e : Error) -> ErrorCode = Prim.errorCode;

  /// Returns the message of an error `e`.
  public let message : (e : Error) -> Text = Prim.errorMessage;

}
