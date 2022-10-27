import Prim "mo:prim";

actor a {
    let length = 100_000;
    
    var array1: [var Nat] = [var];

    func runArrayInit(): async () {
        array1 := Prim.Array_init<Nat>(length, 42);
    };

    func verifyArrayInit() {
        assert array1.size() == length;
        var index = 0;
        while (index < length) {
            assert array1[index] == 42;
            index += 1
        }
    };

    var array2: [Nat] = [];

    func runArrayTabulate(): async () {
        array2 := Prim.Array_tabulate<Nat>(length, func i { 1 + 2 * i });
    };

    func verifyArrayTabulate() {
        assert array2.size() == length;
        var index = 0;
        while (index < length) {
            assert array2[index] == 1 + 2 * index;
            index += 1
        }
    };

    public func run(): async() {
        await runArrayInit();
        Prim.debugPrint("Ignore Diff: Array init: " # debug_show(Prim.rts_mutator_instructions()) # " instructions");
        verifyArrayInit();

        await runArrayTabulate();
        Prim.debugPrint("Ignore Diff: Array tabulate: " # debug_show(Prim.rts_mutator_instructions()) # " instructions");
        verifyArrayTabulate();
    }
};

a.run(); //OR-CALL ingress run 0x4449444C0000
