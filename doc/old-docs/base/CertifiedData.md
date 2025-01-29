# CertifiedData
Certified data.

The Internet Computer allows canister smart contracts to store a small amount of data during
update method processing so that during query call processing, the canister can obtain
a certificate about that data.

This module provides a _low-level_ interface to this API, aimed at advanced
users and library implementors. See the Internet Computer Functional
Specification and corresponding documentation for how to use this to make query
calls to your canister tamperproof.

## Value `set`
``` motoko no-repl
let set : (data : Blob) -> ()
```

Set the certified data.

Must be called from an update method, else traps.
Must be passed a blob of at most 32 bytes, else traps.

Example:
```motoko no-repl
import CertifiedData "mo:base/CertifiedData";
import Blob "mo:base/Blob";

// Must be in an update call

let array : [Nat8] = [1, 2, 3];
let blob = Blob.fromArray(array);
CertifiedData.set(blob);
```

See a full example on how to use certified variables here: https://github.com/dfinity/examples/tree/master/motoko/cert-var


## Value `getCertificate`
``` motoko no-repl
let getCertificate : () -> ?Blob
```

Gets a certificate

Returns `null` if no certificate is available, e.g. when processing an
update call or inter-canister call. This returns a non-`null` value only
when processing a query call.

Example:
```motoko no-repl
import CertifiedData "mo:base/CertifiedData";
// Must be in a query call

CertifiedData.getCertificate();
```
See a full example on how to use certified variables here: https://github.com/dfinity/examples/tree/master/motoko/cert-var

