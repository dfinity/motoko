//ENHANCED-ORTHOGONAL-PERSISTENCE-ONLY
import Prim "mo:prim";

actor {
    func genericFunction1<T, U <: T>(x : U) : ?T {
        Prim.debugPrint("GENERIC FUNCTION 1");
        ?x;
    };

    stable let f1 = genericFunction1;
    assert (f1<Int, Nat>(1) == ?1);

    func genericFunction2<T, U <: Nat and Int>(x : T, u : U) : ?T {
        Prim.debugPrint("GENERIC FUNCTION 2");
        ?x;
    };

    stable let f2 = genericFunction2;
    assert (f2<Int, Nat>(0, 1) == ?0);
};
