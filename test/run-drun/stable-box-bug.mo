//ENHANCED-ORTHOGONAL-PERSISTENCE-ONLY
import Prim "mo:prim";

// counter-example, demonstrating unsoundness of promoting a generic stable function to a generic non-stable function.
// Type arguments are restricted to stable types for stable functions, but
// unrestricted for non-stable functions, so you can avoid the restriction
// by promoting the function to a non-stable super type before application.

//
// Possible fixes:
// * Only allow type parameters to range over stable types, both for local and stable functions.
//   Probably too draconian. Not backwards compatible.
// * Add a type parameter sort for stable type (similar to equality type parameters in SML/restricted type classes).
// * Consider a stable-sorted function type stable only if, when applied to stable
//   type parameters, its arguments and results are stable types too. That might
//   rule out the map field below, because it's type is actually not stable,
//   eventhough it contains just stable-sorted functions.

/*
[nix-shell:~/motoko/test/run-drun]$ EXTRA_MOC_ARGS=--enhanced-orthogonal-persistence ../run.sh -d  stable-box-bug.mo
WARNING: Could not run ic-ref-run, will skip running some tests
stable-class-bug: [tc] [comp] [comp-ref] [valid] [valid-ref] [drun-run]
All tests passed.

[nix-shell:~/motoko/test/run-drun]$ moc --enhanced-orthogonal-persistence --stable-types stable-box-bug.mo
stable-box-bug.mo:2.8-2.12: warning [M0194], unused identifier Prim (delete or rename to wildcard `_` or `_Prim`)

[nix-shell:~/motoko/test/run-drun]$ more stable-box-bug.most
// Version: 1.0.0
actor {
  stable map : {get : stable () -> () -> (); put : stable (() -> ()) -> ()}
};

*/

persistent actor {
    class Box<T>(v : T) {
        var value = v;
        public func get() : T = v;
        public func put(v : T) = value := v
    };

// correctly rejected, T is non-stable
//    let box = Box<() -> ()>(func () {});
//    box.put(func () {});

    // a non-stable super-type of the class constructor type
    type Super = <T>(v : T) ->
    { put : stable T -> ();
      get : stable ()-> T
    };

    transient let SuperBox = Box : Super;
    // incorrectly allowed, though T is non-stable
    let map = SuperBox<() -> ()>(func () {});
    map.put(func () {});
    map.get()();

};

//SKIP run
//SKIP run-ir
//SKIP run-low

//CALL upgrade ""
