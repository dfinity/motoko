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

## Value `reject`
``` motoko no-repl
let reject : (message : Text) -> Error
```

Create an error from the message with the code `#canister_reject`.

Example:
```motoko
import Error "mo:base/Error";

Error.reject("Example error") // can be used as throw argument
```

## Value `code`
``` motoko no-repl
let code : (error : Error) -> ErrorCode
```

Returns the code of an error.

Example:
```motoko
import Error "mo:base/Error";

let error = Error.reject("Example error");
Error.code(error) // #canister_reject
```

## Value `message`
``` motoko no-repl
let message : (error : Error) -> Text
```

Returns the message of an error.

Example:
```motoko
import Error "mo:base/Error";
import Debug "mo:base/Debug";

let error = Error.reject("Example error");
Error.message(error) // "Example error"
```
