actor /*class () =*/ Self {
    func caller(_callee : shared () -> async ()) { };
    caller(Self.method1); // FIXME: should be error too
    caller(method1);
    public func method1() : async () { };
}
