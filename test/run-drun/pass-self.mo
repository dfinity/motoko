import { debugPrint } = "mo:⛔";

actor Self {
    public func method() : async () { debugPrint "YESS!" };

    var c : ?(shared () -> async ()) = null;
    func caller(callee : shared () -> async ()) { c := ?callee };

    caller(Self.method)
}
