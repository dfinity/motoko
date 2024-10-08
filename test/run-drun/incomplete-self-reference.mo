import { debugPrint } = "mo:⛔";

actor Self {
    var stored : shared () -> async () = Self.method;
    public func method() : async () { debugPrint "Hey!" };
    public func go() : async () { await stored() };
};

Self.go(); //OR-CALL ingress go "DIDL\x00\x00"
