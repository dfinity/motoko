import { debugPrint; setTimer } = "mo:⛔";

// no send capability: must not call `setTimer`
func _bowm() {
    ignore setTimer(1_000_000, false, func () : async () { });
};

// transferred send capability: may call `setTimer`
func _gawd<async>() {
    ignore setTimer(1_000_000, false, func () : async () { });

    debugPrint<async>("caveat"); // misplaced `<async>`

    ignore async 42 // not allowed
};

// these are allowed to contain sends
func _eeek<async>() : async () {
    ignore setTimer(1_000_000, false, func () : async () { });

    ignore await async 42
};

func _gwerr() : async Int = async {
    ignore setTimer(1_000_000, false, func () : async () { });

    await async 42
};
