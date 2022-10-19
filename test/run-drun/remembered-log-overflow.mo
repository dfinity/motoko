import Prim "mo:prim";

actor {
    type Node = ?{ var value: Nat };

    type Pair = {
        var x: Node;
        var y: Node;
    };

    var root: ?Pair = null;

    public func first_call() : async() {
        root := ?{
            var x = null;
            var y = null;
        }
    };

    public func second_call(): async() {
        switch root {
            case null Prim.trap("Missing root");
            case (?root) {
                root.x := ?{ var value = 1 };
                root.y := ?{ var value = 2 };
                var i = 0;
                while (i < 10_000_000) {
                    let temp = root.x;
                    root.x := root.y;
                    root.y := temp;
                    i += 1
                }
            }
        }
    }
};

// too slow for ic-ref-run
//SKIP run-low
//SKIP run-ir
//SKIP ic-ref-run

//CALL ingress first_call "DIDL\x00\x00"
//CALL ingress second_call "DIDL\x00\x00"
