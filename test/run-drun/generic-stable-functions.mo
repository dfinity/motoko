//ENHANCED-ORTHOGONAL-PERSISTENCE-ONLY
import Prim "mo:prim";

persistent actor {
    persistent func genericFunction1<T, U <: T>(x : U) : ?T {
        Prim.debugPrint("GENERIC FUNCTION 1");
        ?x;
    };

    let f1 = genericFunction1;
    assert (f1<Int, Nat>(1) == ?1);

    persistent func genericFunction2<T, U <: Nat and Int>(x : T, u : U) : ?T {
        Prim.debugPrint("GENERIC FUNCTION 2");
        ?x;
    };

    let f2 = genericFunction2;
    assert (f2<Int, Nat>(0, 1) == ?0);

    persistent class Test<T>() {
        public func genericFunction3<U <: T>(x : U) : ?T {
            Prim.debugPrint("GENERIC FUNCTION 3");
            ?x;
        }
    };
    let f3 = Test<Int>().genericFunction3;
    assert (f3<Nat>(1) == ?1);
};

//SKIP run
//SKIP run-low
//SKIP run-ir
//SKIP comp-ref
//CALL upgrade ""
