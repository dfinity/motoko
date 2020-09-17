type List<T> = ?{head: T; tail : List<T>};
actor class (init : List<Int>) {
    var list = init;
    public func get() : async List<Int> { list };
};
