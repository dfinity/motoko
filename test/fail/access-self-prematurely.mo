actor /*class () =*/ Self {
    func caller(_callee : shared () -> async ()) { };
    caller(method1);
    public func method1() : async () { };
}
