//ENHANCED-ORTHOGONAL-PERSISTENCE-ONLY
import Prim "mo:prim";

persistent actor {
    class Box<T>(v : T) {
        var value = v;
        public func get() : T = v;
        public func put(v : T) = value := v;
    };

    let box = Box<() -> ()>(func() {});
    box.put(func() {});
};

//SKIP run
//SKIP run-ir
//SKIP run-low

//CALL upgrade ""
