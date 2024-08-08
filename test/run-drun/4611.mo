import Prim "mo:⛔";
actor A {
    public func method() {};
    var bool = true;
    let text = "Hello World!";
    let char = 'C';
    stable let rec = { bool; text };
    stable let mrec = { var bool; var text };
    bool := false;
    assert mrec.bool;  // check no aliasing between bool and mrec.bool
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
            record = { rec; bool };
            recdot = rec.text;
            objdot = (object { let (a, c) = (42, 25); public let b = a }).b;
            tuple = (rec.text, char);
            tupledot = (rec.text, char).1;
            option = ?char;
            array = [char, 'D'];
            //actordot = A.method : shared () -> ();
            tag = #tag rec;
            annot = ?char : ?Char;
            ite = if bool text else "Nope";
            labeled = label l ();
            function = func () = ();
        }.char
    };
    let summary = debug_show { rec; mrec; nrec };
    Prim.debugPrint summary;
    // check that all executions work the same way
    Prim.debugPrint (debug_show { bool; aool = (bool := false) });
    Prim.debugPrint (debug_show { bool; cool = (bool := true) });
}
