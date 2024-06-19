// stable memory

// (extracted to a separate file so we can map `@` to `_` to disable warning for --experimental-stable-memory n, n > 1)

/// @deprecated Please use the Region library instead: https://internetcomputer.org/docs/current/motoko/main/stable-memory/stable-regions/#the-region-library or compile with flag `--experimental-stable-memory 2` to disable this warning.
func stableMemorySize() : Nat64 = (prim "stableMemorySize" : () -> Nat64)();

/// @deprecated Please use the Region library instead: https://internetcomputer.org/docs/current/motoko/main/stable-memory/stable-regions/#the-region-library or compile with flag `--experimental-stable-memory 2` to disable this warning.
func stableMemoryGrow(pages : Nat64) : Nat64 = (prim "stableMemoryGrow" : Nat64 -> Nat64) pages;

/// @deprecated Please use the Region library instead: https://internetcomputer.org/docs/current/motoko/main/stable-memory/stable-regions/#the-region-library or compile with flag `--experimental-stable-memory 2` to disable this warning.
func stableMemoryLoadNat32(offset : Nat64) : Nat32 = (prim "stableMemoryLoadNat32" : Nat64 -> Nat32) offset;

/// @deprecated Please use the Region library instead: https://internetcomputer.org/docs/current/motoko/main/stable-memory/stable-regions/#the-region-library or compile with flag `--experimental-stable-memory 2` to disable this warning.
func stableMemoryStoreNat32(offset : Nat64, val : Nat32) : () = (prim "stableMemoryStoreNat32" : (Nat64, Nat32) -> ())(offset, val);

/// @deprecated Please use the Region library instead: https://internetcomputer.org/docs/current/motoko/main/stable-memory/stable-regions/#the-region-library or compile with flag `--experimental-stable-memory 2` to disable this warning.
func stableMemoryLoadNat8(offset : Nat64) : Nat8 = (prim "stableMemoryLoadNat8" : Nat64 -> Nat8) offset;

/// @deprecated Please use the Region library instead: https://internetcomputer.org/docs/current/motoko/main/stable-memory/stable-regions/#the-region-library or compile with flag `--experimental-stable-memory 2` to disable this warning.
func stableMemoryStoreNat8(offset : Nat64, val : Nat8) : () = (prim "stableMemoryStoreNat8" : (Nat64, Nat8) -> ())(offset, val);

/// @deprecated Please use the Region library instead: https://internetcomputer.org/docs/current/motoko/main/stable-memory/stable-regions/#the-region-library or compile with flag `--experimental-stable-memory 2` to disable this warning.
func stableMemoryLoadNat16(offset : Nat64) : Nat16 = (prim "stableMemoryLoadNat16" : Nat64 -> Nat16) offset;

/// @deprecated Please use the Region library instead: https://internetcomputer.org/docs/current/motoko/main/stable-memory/stable-regions/#the-region-library or compile with flag `--experimental-stable-memory 2` to disable this warning.
func stableMemoryStoreNat16(offset : Nat64, val : Nat16) : () = (prim "stableMemoryStoreNat16" : (Nat64, Nat16) -> ())(offset, val);

/// @deprecated Please use the Region library instead: https://internetcomputer.org/docs/current/motoko/main/stable-memory/stable-regions/#the-region-library or compile with flag `--experimental-stable-memory 2` to disable this warning.
func stableMemoryLoadNat64(offset : Nat64) : Nat64 = (prim "stableMemoryLoadNat64" : Nat64 -> Nat64) offset;

/// @deprecated Please use the Region library instead: https://internetcomputer.org/docs/current/motoko/main/stable-memory/stable-regions/#the-region-library or compile with flag `--experimental-stable-memory 2` to disable this warning.
func stableMemoryStoreNat64(offset : Nat64, val : Nat64) : () = (prim "stableMemoryStoreNat64" : (Nat64, Nat64) -> ())(offset, val);

/// @deprecated Please use the Region library instead: https://internetcomputer.org/docs/current/motoko/main/stable-memory/stable-regions/#the-region-library or compile with flag `--experimental-stable-memory 2` to disable this warning.
func stableMemoryLoadInt32(offset : Nat64) : Int32 = (prim "stableMemoryLoadInt32" : Nat64 -> Int32) offset;

/// @deprecated Please use the Region library instead: https://internetcomputer.org/docs/current/motoko/main/stable-memory/stable-regions/#the-region-library or compile with flag `--experimental-stable-memory 2` to disable this warning.
func stableMemoryStoreInt32(offset : Nat64, val : Int32) : () = (prim "stableMemoryStoreInt32" : (Nat64, Int32) -> ())(offset, val);

/// @deprecated Please use the Region library instead: https://internetcomputer.org/docs/current/motoko/main/stable-memory/stable-regions/#the-region-library or compile with flag `--experimental-stable-memory 2` to disable this warning.
func stableMemoryLoadInt8(offset : Nat64) : Int8 = (prim "stableMemoryLoadInt8" : Nat64 -> Int8) offset;

/// @deprecated Please use the Region library instead: https://internetcomputer.org/docs/current/motoko/main/stable-memory/stable-regions/#the-region-library or compile with flag `--experimental-stable-memory 2` to disable this warning.
func stableMemoryStoreInt8(offset : Nat64, val : Int8) : () = (prim "stableMemoryStoreInt8" : (Nat64, Int8) -> ())(offset, val);

/// @deprecated Please use the Region library instead: https://internetcomputer.org/docs/current/motoko/main/stable-memory/stable-regions/#the-region-library or compile with flag `--experimental-stable-memory 2` to disable this warning.
func stableMemoryLoadInt16(offset : Nat64) : Int16 = (prim "stableMemoryLoadInt16" : Nat64 -> Int16) offset;

/// @deprecated Please use the Region library instead: https://internetcomputer.org/docs/current/motoko/main/stable-memory/stable-regions/#the-region-library or compile with flag `--experimental-stable-memory 2` to disable this warning.
func stableMemoryStoreInt16(offset : Nat64, val : Int16) : () = (prim "stableMemoryStoreInt16" : (Nat64, Int16) -> ())(offset, val);

/// @deprecated Please use the Region library instead: https://internetcomputer.org/docs/current/motoko/main/stable-memory/stable-regions/#the-region-library or compile with flag `--experimental-stable-memory 2` to disable this warning.
func stableMemoryLoadInt64(offset : Nat64) : Int64 = (prim "stableMemoryLoadInt64" : Nat64 -> Int64) offset;

/// @deprecated Please use the Region library instead: https://internetcomputer.org/docs/current/motoko/main/stable-memory/stable-regions/#the-region-library or compile with flag `--experimental-stable-memory 2` to disable this warning.
func stableMemoryStoreInt64(offset : Nat64, val : Int64) : () = (prim "stableMemoryStoreInt64" : (Nat64, Int64) -> ())(offset, val);

/// @deprecated Please use the Region library instead: https://internetcomputer.org/docs/current/motoko/main/stable-memory/stable-regions/#the-region-library or compile with flag `--experimental-stable-memory 2` to disable this warning.
func stableMemoryLoadFloat(offset : Nat64) : Float = (prim "stableMemoryLoadFloat" : Nat64 -> Float) offset;

/// @deprecated Please use the Region library instead: https://internetcomputer.org/docs/current/motoko/main/stable-memory/stable-regions/#the-region-library or compile with flag `--experimental-stable-memory 2` to disable this warning.
func stableMemoryStoreFloat(offset : Nat64, val : Float) : () = (prim "stableMemoryStoreFloat" : (Nat64, Float) -> ())(offset, val);

/// @deprecated Please use the Region library instead: https://internetcomputer.org/docs/current/motoko/main/stable-memory/stable-regions/#the-region-library or compile with flag `--experimental-stable-memory 2` to disable this warning.
func stableMemoryLoadBlob(offset : Nat64, size : Nat) : Blob = (prim "stableMemoryLoadBlob" : (Nat64, Nat) -> Blob)(offset, size);

/// @deprecated Please use the Region library instead: https://internetcomputer.org/docs/current/motoko/main/stable-memory/stable-regions/#the-region-library or compile with flag `--experimental-stable-memory 2` to disable this warning.
func stableMemoryStoreBlob(offset : Nat64, val : Blob) : () = (prim "stableMemoryStoreBlob" : (Nat64, Blob) -> ())(offset, val);

