//ENHANCED-ORTHOGONAL-PERSISTENCE-ONLY
import Prim "mo:prim";

persistent actor {
    // a stable type
    class Box<T>(v : T) {
        var value = v;
        public func get() : T = v;
        public func put(v : T) = value := v
    };

    // a flexible type
    type Super = <T>(v : T) ->
    { put : persistent T -> ();
      get : persistent ()-> T
    };

    // rejected due generic type parameter invariance
    // stable type parameters cannot become flexible
    transient let SuperBox = Box : Super;
    
    let map = SuperBox<() -> ()>(func () {});
    map.put(func () {});
    map.get()();
};

//SKIP run
//SKIP run-ir
//SKIP run-low

//CALL upgrade ""
