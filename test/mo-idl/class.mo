import L "List";
actor class (init : L.List<Nat>) {
    var list = init;
    public func get() : async L.List<Int> { list };
};
