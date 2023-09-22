import P "mo:⛔";
import M "../stable-mem/StableMemory";

actor {

    // measure out three blocks' worth of bytes.
    let pageInBytes = 1 << 16 : Nat64;
    let blockInBytes = pageInBytes * 128 : Nat64;
    let size = blockInBytes * 3 : Nat64;
    var i = 0 : Nat64;
    var b = 0 : Nat8;

    // Grow to the necessary number of pages.
    let reqPages = size / pageInBytes;

    P.debugPrint("reqPages = " # (debug_show reqPages));

    assert M.grow(reqPages) == 0;
    assert M.size() == reqPages;

    // write byte pattern, in a defined interval.
    while (i < size) {
        M.storeNat8(i, b);
        i := i + 10;
        b := b +% 1;
    };

    P.debugPrint ("actor0: init'ed.");
}
