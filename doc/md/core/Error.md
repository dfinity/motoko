# core/Error
Error values and inspection.

The `Error` type is the argument to `throw`, parameter of `catch`.
The `Error` type is opaque.

## Type `Error`
``` motoko no-repl
type Error = Prim.Types.Error
```

Error value resulting from  `async` computations

## Type `ErrorCode`
``` motoko no-repl
type ErrorCode = Prim.ErrorCode
```

Error code to classify different kinds of user and system errors:
```motoko
type ErrorCode = {
  // Fatal error.
  #system_fatal;
  // Transient error.
  #system_transient;
  // Destination invalid.
  #destination_invalid;
  // Canister error (e.g., trap, no response).
  #canister_error;
  // Explicit reject by canister code.
  #canister_reject;
  // Response unknown; system stopped waiting for it (e.g., timed out, or system under high load).
  #system_unknown;
  // Future error code (with unrecognized numeric code).
  #future : Nat32;
  // Error issuing inter-canister call
  // (indicating destination queue full or freezing threshold crossed).
  #call_error : { err_code :  Nat32 }
};
```

## Function `reject`
``` motoko no-repl
func reject(message : Text) : Error
```

Create an error from the message with the code `#canister_reject`.

Example:
```motoko
import Error "mo:core/Error";

Error.reject("Example error") // can be used as throw argument
```

## Function `code`
``` motoko no-repl
func code(self : Error) : ErrorCode
```

Returns the code of an error.

Example:
```motoko
import Error "mo:core/Error";

let error = Error.reject("Example error");
Error.code(error) // #canister_reject
```

## Function `message`
``` motoko no-repl
func message(self : Error) : Text
```

Returns the message of an error.

Example:
```motoko
import Error "mo:core/Error";

let error = Error.reject("Example error");
Error.message(error) // "Example error"
```

## Function `isCleanReject`
``` motoko no-repl
func isCleanReject(self : Error) : Bool
```

Checks if the error is a clean reject.
A clean reject means that there must be no state changes on the callee side.

## Function `isRetryPossible`
``` motoko no-repl
func isRetryPossible(self : Error) : Bool
```

Returns whether retrying to send a message may result in success.

Example:
```motoko
import Error "mo:core/Error";
import Debug "mo:core/Debug";

persistent actor {
  type CallableActor = actor {
    call : () -> async ()
  };

  public func example(callableActor : CallableActor) {
    try {
      await (with timeout = 3) callableActor.call();
    }
    catch e {
      if (Error.isRetryPossible e) {
        Debug.print(Error.message e);
      }
    }
  }
}

```
