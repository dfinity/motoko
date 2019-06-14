type Cast< A <: B, B> = A -> B;

let f = (func(x:Int):Nat = 0) : Cast<Int,Nat>;

func f2a< A <: B, B> ( x : A ) : B = x;

func f2b< B, A <: B> ( x : A ) : B = x;

func f3a< A <: B, B /* <: C */ , C> ( x : A ) : C = x;
func f3b< B /* <: C */, A <: B, C> ( x : A ) : C = x;
func f3c< C,  B /* <: C */, A <: B> ( x : A ) : C = x;

type T1< A <: B, B /* <: C */, C> = Cast<A,C>;
type T2<  B /* <: C */, A <: B, C> = Cast<A,C>;
type T3< C, B /* <: C */, A <: B> = Cast<A,C>;

func g< A <: B, B /* <: C */ , C> ( a : A, b : B, c: C, n:Int ) : C {
 type U0 = T1<A,A,A>;
 type U1 = T1<A,A,B>;
 type U2 = T1<A,B,B>;
 type U3 = T1<B,B,B>;
 type U4 = T1<B,B,C>;
 type U5 = T1<B,C,C>;
 type U6 = T1<C,C,C>;

 type V0 = T2<A,A,A>;
 type V1 = T2<A,A,B>;
 type V2 = T2<B,A,B>;
 type V3 = T2<B,B,B>;
 type V4 = T2<B,B,C>;
 type V5 = T2<C,B,C>;
 type V6 = T2<C,C,C>;

 type W0 = T3<A,A,A>;
 type W1 = T3<B,A,A>;
 type W2 = T3<B,B,A>;
 type W3 = T3<B,B,B>;
 type W4 = T3<C,B,B>;
 type W5 = T3<C,C,B>;
 type W6 = T3<C,C,C>;


 switch n {
    case 0 { g<A,B,C>(a,a,a,n) };
    case 1 { g<A,B,C>(a,b,b,n) };
    case 2 { g<A,B,C>(a,b,c,n) };

    case 3 { g<B,B,C>(a,a,a,n) };
    case 4 { g<B,B,C>(a,b,b,n) };
    case 5 { g<B,B,C>(a,b,c,n) };
    case 6 { g<B,B,C>(b,b,b,n) };
    case 7 { g<B,B,C>(b,b,c,n) };

    case 8 { g<C,C,C>(a,a,a,n) };
    case 9 { g<C,C,C>(a,a,b,n) };
    case 10 { g<C,C,C>(a,b,c,n) };
    case 11 { g<C,C,C>(c,c,c,n) };
    case _  { c };
 }
};

