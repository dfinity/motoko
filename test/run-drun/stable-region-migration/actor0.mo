import P "mo:⛔";
import M "../stable-mem/StableMemory";

actor FillUp {

    // measure out three blocks' worth of bytes.
    let pageInBytes = 1 << 16 : Nat64;
    let blockInBytes = pageInBytes * 128 : Nat64;
    let size = blockInBytes * 3 : Nat64;
    var i = 0 : Nat64;

    // Grow to the necessary number of pages.
    let reqPages = size / pageInBytes;
    assert M.grow(reqPages) == 0;
    assert M.size() == reqPages;
    
    // write byte pattern, one byte at a time.
    while (i < size) {
        let byte_pattern = P.natToNat8(P.nat64ToNat(i % 256)) : Nat8;
        M.storeNat8(i, byte_pattern);
        i := i + 1;
    };

    P.debugPrint ("actor0: init'ed.");
}
