import Prim "mo:⛔";

actor {
    var bool = true;
    let text = "Hello World!";
    let char = 'C';
    stable let rec = { bool; text };
    stable let mrec = { var bool; var text };
    // projecting a field from a var-built record
    // sometimes the compiler will do this
    func call() : Bool { Prim.debugPrint "Effect"; bool };
    stable let nrec = {
        bool = { text; bool }.bool;
        text = { text }.text;
        char = {
            char; // relevant
            fuzz = "Noise";
            effectful = call();
            evaluated = rec;
            recdot = rec.text
        }.char
    };
    let summary = debug_show { rec; mrec; nrec };
    Prim.debugPrint summary;
}
