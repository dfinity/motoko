//MOC-FLAG --force-gc
import { performanceCounter; rts_heap_size; debugPrint } = "mo:⛔";

actor Palindrome {
    var steps = 0;
    var nat = 3;

    func counters() : (Int, Nat64) = (rts_heap_size(), performanceCounter(0));

    let arr : [var Nat] = [var 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0];

    public func go() : async () {
        let (size0, perf0) = counters();

        while (steps < 11) {
            nat := nat * 5 * nat;
            arr[steps] := nat;
            steps := steps + 1
        };

        let (size1, perf1) = counters();

        debugPrint(debug_show {size = size1 - size0; cycles = perf1 - perf0 });
    }
}

//CALL ingress go 0x4449444C0000
