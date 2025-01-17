//CLASSICAL-PERSISTENCE-ONLY
// test pretty printing of scoped functions (indirectly via error messages)
(func f1<A>(g:A -> async ()):(){}) ();

(func f2<A, B>(g:(A,B) -> async ()):(){}) ();

(func f3<A, B, C>(g:(A,B,C) -> async ()):(){}) ();

(func f4<A, B, C>(g:<D>(A,B,C,D) -> async ()):(){}) ();

(func f5<A, B, C>(g:<D <: C,E <: D>(A,B,C,D,E) -> async ()):(){}) ();


