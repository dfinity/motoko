import { debugPrint; setTimer } = "mo:⛔";

func() {

    func indirect<system>() {
        ignore setTimer<system>(0, false, func () : async () { debugPrint "YEP!" });
    };

    indirect<system>()
}
