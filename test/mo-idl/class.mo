import L "../run/lib/ListM";
actor class (init : L.List<Int>) {
    var list = init;
    public func get() : async L.List<Int> { list };
};
