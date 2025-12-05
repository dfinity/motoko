// test pretty printing of scoped functions (indirectly via error messages)
(func f1<A>(g : A -> async ()) : () {}) 1;

(func f2<A, B>(g : (A, B) -> async ()) : () {}) 1;

(func f3<A, B, C>(g : (A, B, C) -> async ()) : () {}) 1;

(func f4<A, B, C>(g : <D>(A, B, C, D) -> async ()) : () {}) 1;

(func f5<A, B, C>(g : <D <: C, E <: D>(A, B, C, D, E) -> async ()) : () {}) 1;
