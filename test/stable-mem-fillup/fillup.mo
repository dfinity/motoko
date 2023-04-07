import M "mo:base/ExperimentalStableMemory";
import I "mo:base/Iter";
import Nat64 "mo:base/Nat64";
import Nat32 "mo:base/Nat32";
import Nat8 "mo:base/Nat8";

actor FillUp {

    // measure out three blocks' worth of bytes.
    let pageInBytes = 1 << 16 : Nat32;
    let blockInBytes = pageInBytes * 128 : Nat32;
    let size = blockInBytes * 3 : Nat32;

    // write byte pattern, one byte at a time.
    for (i in I.range(0, Nat32.toNat(size))) {
        let byte_pattern = i % 256;
        M.storeNat8(
          Nat64.fromNat(i),
          Nat8.fromNat(byte_pattern)
        )
    }
}
