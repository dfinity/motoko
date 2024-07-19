import Prim "mo:⛔";

actor {
    var bool = true;
    let text = "Hello World!";
    stable let rec = { bool; text };
    stable let mrec = { var bool; var text };
    bool := false;
    assert mrec.bool;  // check no aliasing between bool and mrec.bool
    let summary = debug_show { rec; mrec };
    Prim.debugPrint summary;
    // check that all executions work the same way
    Prim.debugPrint (debug_show { bool; aool = (bool := false) });
    Prim.debugPrint (debug_show { bool; cool = (bool := true) });
}
