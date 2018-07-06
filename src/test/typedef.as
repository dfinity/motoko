type IntT = Int;
type Arrow<T,U> = T->U;
type Compose<T,U,V> = (T->U) -> (U->V) -> T -> V;
type Id = <T> T->T;
type Seq<A> = (A,Seq<A>?);
type A = (B,A);
type B = (A,B);
