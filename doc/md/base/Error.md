# Error
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
  // Response unknown due to missed deadline.
  #system_unknown;
  // Destination invalid.
  #destination_invalid;
  // Explicit reject by canister code.
  #canister_reject;
  // Canister trapped.
  #canister_error;
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
import Error "mo:base/Error";

Error.reject("Example error") // can be used as throw argument
```

## Function `code`
``` motoko no-repl
func code(error : Error) : ErrorCode
```

Returns the code of an error.

Example:
```motoko
import Error "mo:base/Error";

let error = Error.reject("Example error");
Error.code(error) // #canister_reject
```

## Function `message`
``` motoko no-repl
func message(error : Error) : Text
```

Returns the message of an error.

Example:
```motoko
import Error "mo:base/Error";

let error = Error.reject("Example error");
Error.message(error) // "Example error"
```

## Function `isRetryPossible`
``` motoko no-repl
func isRetryPossible(error : Error) : Bool
```

Returns whether retrying to send a message may result in success.

Example:
```motoko
import { message; isRetryPossible } "mo:base/Error";
import { print } "mo:base/Debug";

try await (with timeout = 3) Actor.call(arg)
catch e { if (isRetryPossible e) print(message e) }
```
