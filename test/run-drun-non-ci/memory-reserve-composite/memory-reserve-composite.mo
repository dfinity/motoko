import Prim "mo:⛔";
actor {
    stable var stableData = Prim.Array_tabulate<Nat>(1024 * 1024, func(index) { index });
    var array0 : [var Nat] = [var];
    var array1 : [var Nat] = [var];
    var array2 : [var Nat] = [var];
    var array3 : [var Nat] = [var];
    Prim.debugPrint("Initialized " # debug_show (Prim.rts_memory_size()));

    public func prepare1() : async () {
        array0 := Prim.Array_init<Nat>(256 * 1024 * 1024, 0); // 1GB
        array1 := Prim.Array_init<Nat>(256 * 1024 * 1024, 1); // 2GB
        Prim.debugPrint("Prepared1 " # debug_show (Prim.rts_memory_size()));
    };

    public func prepare2() : async () {
        array2 := Prim.Array_init<Nat>(256 * 1024 * 1024, 2); // 3GB
        array3 := Prim.Array_init<Nat>(150 * 1024 * 1024, 3); // around 3.75GB
        Prim.debugPrint("Prepared2 " # debug_show (Prim.rts_memory_size()));
    };

    public composite query func allocateInCompositeQuery() : async () {
        ignore Prim.Array_init<Nat>(50 * 1024 * 1024, 4);
        Prim.debugPrint("Composite query call " # debug_show (Prim.rts_memory_size()));
        assert (Prim.rts_memory_size() > 3840 * 1024 * 1024);
        await nestedQuery();
        ignore Prim.Array_init<Nat>(5 * 1024 * 1024, 4);
        Prim.debugPrint("Composite query callback " # debug_show (Prim.rts_memory_size()));
        assert (Prim.rts_memory_size() > 3840 * 1024 * 1024);
    };

    public query func nestedQuery() : async () {
        Prim.debugPrint("Nested query " # debug_show (Prim.rts_memory_size()));
    };
};

//SKIP run
//SKIP run-ir
//SKIP run-low
