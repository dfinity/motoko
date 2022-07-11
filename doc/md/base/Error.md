# Error
Error values and inspection.

The `Error` type is the argument to `throw`, parameter of `catch`.
The `Error` type is opaque.

## Type `Error`
`type Error = Prim.Types.Error`

Error values resulting from  `async` computations

## Type `ErrorCode`
`type ErrorCode = Prim.ErrorCode`

Error codes (user and system), where module `Prim` defines:
```motoko
type ErrorCode = {
  // Fatal error.
  #system_fatal;
  // Transient error.
  #system_transient;
  // Destination invalid.
  #destination_invalid;
  // Explicit reject by canister code.
  #canister_reject;
  // Canister trapped.
  #canister_error;
  // Future error code (with unrecognized numeric code)
  #future : Nat32;
};
```

## Value `reject`
`let reject : (m : Text) -> Error`

Create an error from message `m` with code #canister_reject.

## Value `code`
`let code : (e : Error) -> ErrorCode`

Returns the code of an error `e`.

## Value `message`
`let message : (e : Error) -> Text`

Returns the message of an error `e`.
