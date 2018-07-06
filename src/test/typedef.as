type IntT = Int;
type Arrow<T,U> = (T,U);
type Seq<A> = (A,(Seq<A>)?);
type A = (B,A);
type B = (A,B);
