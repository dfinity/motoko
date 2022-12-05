import Prim "mo:prim";
import Iter "base/Iter";
import Buffer "base/Buffer";

actor {
    let buffer = Buffer.Buffer<Nat>(8);

    func populate(amount: Nat) {
        Prim.debugPrint("Buffer populate " # debug_show(amount));
        for (count in Iter.range(0, amount - 1)) {
            buffer.add(count)
        }
    };

    func traverse() {
        Prim.debugPrint("Buffer traverse " # debug_show(buffer.size()));
        for (value in buffer.vals()) {
            ignore value
        }
    };

    func discard(amount: Nat) {
        Prim.debugPrint("Buffer discard " # debug_show(amount));
        for (count in Iter.range(0, amount - 1)) {
            ignore buffer.removeLast()
        }
    };

    func clear() {
        Prim.debugPrint("Buffer clear");
        buffer.clear()
    };

    public shared func run(): async () {
        for (index in Iter.range(0, 3)) {
            populate(100_000);
        };
        traverse();
        discard(100_000);
        discard(100_000);
        for (index in Iter.range(0, 5)) {
            populate(100_000);
        }
    };
}

//CALL ingress run 0x4449444C0000