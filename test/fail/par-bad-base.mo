class migration({}) = {};

(with migration; moot = "MOOT")
actor {
    func a() : async () {
        await ({ cycles = -3 } with) async ();
        await ({ cycles = "" } with) async ();
    };
};
