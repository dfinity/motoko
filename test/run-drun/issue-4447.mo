import { debugPrint; setTimer } = "mo:⛔";

class <system>() {

    func indirect<system>() {
        ignore setTimer<system>(0, false, func () : async () { debugPrint "YEPYEP!" });
    };
    
    ignore setTimer<system>(0, false, func () : async () { debugPrint "YEP!" });
    indirect<system>()
}

//SKIP run
//SKIP run-low
//SKIP run-ir
