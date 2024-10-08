import { debugPrint } = "mo:⛔";

actor Self {
    var stored : shared () -> async () = Self.method;
    public func method() : async () { debugPrint "Hey!" };
    public func go() : async () {
        //assert stored == method;
        //assert stored == Self.method;
        await stored()
    };
};

Self.go(); //OR-CALL ingress go "DIDL\x00\x00"
