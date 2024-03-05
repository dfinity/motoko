import { debugPrint; setTimer } = "mo:⛔";

// no system capability: must not call `setTimer`
func _bowm() {
    ignore setTimer(1_000_000, false, func () : async () { });
};

// transferred system capability: may call `setTimer`
func _gawd<system>() {
    ignore setTimer(1_000_000, false, func () : async () { });

    debugPrint<system>("caveat"); // misplaced `<system>`

    ignore async 42 // not allowed
};

// these are allowed to contain sends
func _eeek<system>() : async () {
    ignore setTimer(1_000_000, false, func () : async () { });

    ignore await async 42
};

func _gwerr() : async Int = async {
    ignore setTimer(1_000_000, false, func () : async () { });

    await async 42
};
