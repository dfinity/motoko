import Prim "mo:⛔";

actor {
    var bool = true;
    let text = "Hello World!";
    stable let rec = { bool; text };
    stable let mrec = { var bool; var text };
    // projecting a field from a var-built record
    // sometimes the compiler will do this
    stable let nrec = { bool = { text; bool }.bool; text = { text }.text };
    let summary = debug_show { rec; mrec; nrec };
    Prim.debugPrint summary;
}
