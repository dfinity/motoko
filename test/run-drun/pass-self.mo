import { debugPrint; principalOfActor } = "mo:⛔";

actor /*class () =*/ Self {
    public func method() : async () { debugPrint "YESS!"; ignore principalOfActor Self };
    func caller(_callee : shared () -> async ()) { };

    debugPrint "BEFORE!";
    ignore principalOfActor Self;
    caller(Self.method);
    caller(method);
    debugPrint (debug_show(principalOfActor Self));
    debugPrint "So far so good!";

    caller(method1);
    public func method1() : async () { debugPrint "YESS!"; ignore principalOfActor Self };
    
}
