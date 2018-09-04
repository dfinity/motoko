// Object Types

{
type A = {x : Int};
type B = {x : Int};
func f(x : A) : A = x : B;
};

{
type A = {x : Int; y : Bool};
type B = {y : Bool; x : Int};
func f(x : A) : A = x : B;
};


// Function Types

{
type A = (Int, Bool) -> (Word8, Float);
type B = (Int, Bool) -> (Word8, Float);
func f(x : A) : A = x : B;
};

{
type A = <X> X -> X;
type B = <Y> Y -> Y;
func f(x : A) : A = x : B;
};

{
type A = <X, Y> X -> Y;
type B = <Y, X> Y -> X;
func f(x : A) : A = x : B;
};


// Type Abbreviations

{
type T<X, Y> = X;
type A = T<Int, Bool>;
type B = T<Int, Nat>;
func f(x : A) : A = x : B;
};


// Classes

{
class C<X>() {};
type A = C<Int>;
type B = C<Int>;
func f(x : A) : A = x : B;
};

{
class C<X, Y>() {};
type A<X> = C<X, Int>;
type B<X> = C<X, Int>;
func f<X>(x : A<X>) : A<X> = x : B<X>;
};

{
class C<X>() {};
type T<X, Y> = X;
type A<X> = C<T<X, Int>>;
type B<X> = C<T<X, Bool>>;
func f<X>(x : A<X>) : A<X> = x : B<X>;
};


// Bounds

{
type A = <X <: Int, Y <: {}> X -> Y;
type B = <X <: Int, Y <: {}> X -> Y;
func f(x : A) : A = x : B;
};

{
type T<X, Y> = X;
type A = <X <: Int, Y <: Int> X -> Y;
type B = <X <: Int, Y <: T<Int, Bool>> X -> Y;
func f(x : A) : A = x : B;
};

{
type T<X, Y> = X;
type A = <X <: Int, Y <: X> X -> Y;
type B = <X <: Int, Y <: T<X, Int>> X -> Y;
func f(x : A) : A = x : B;
};


// Recursion

/* TBR: Should this work? It's fine coinductively.
{
type A = A;
type B = B;
func f(x : A) : A = x : B;
};
*/

{
type A = {x : A};
type B = {x : B};
func f(x : A) : A = x : B;
};

{
type A = {x : B};
type B = {x : A};
func f(x : A) : A = x : B;
};

{
type A0 = {x : A0};
type A = {x : A0};
type B = {x : B};
func f(x : A) : A = x : B;
};

{
type A = {x : {x : A}};
type B = {x : B};
func f(x : A) : A = x : B;
};
