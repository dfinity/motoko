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
``` motoko
let set : (data : Blob) -> ()
```

Set the certified data.

Must be called from an update method, else traps.
Must be passed a blob of at most 32 bytes, else traps.

## Value `getCertificate`
``` motoko
let getCertificate : () -> ?Blob
```

Gets a certificate

Returns `null` if no certificate is available, e.g. when processing an
update call or inter-canister call. This returns a non-`null` value only
when processing a query call.
