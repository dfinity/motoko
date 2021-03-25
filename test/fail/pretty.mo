// test pretty printing of small and large types
import Prim "mo:prim";

type T = { aaaaaaaaaaaaaaaaaaa: Nat;
           bbbbbbbbbbbbbbbbbbb: Nat;
           ccccccccccccccccccc: Nat;
           ddddddddddddddddddd: Nat;
           eeeeeeeeeeeeeeeeeee: Nat;
         };

type U = { aaaaaaaaaaaaaaaaaaa: Nat;
           bbbbbbbbbbbbbbbbbbb: Nat;
           ccccccccccccccccccc: Nat;
           ddddddddddddddddddd: Nat;
           eeeeeeeeeeeeeeeeeee: Nat;
         } ->
         { aaaaaaaaaaaaaaaaaaa: Nat;
           bbbbbbbbbbbbbbbbbbb: Nat;
           ccccccccccccccccccc: Nat;
           ddddddddddddddddddd: Nat;
           eeeeeeeeeeeeeeeeeee: Nat;
         };

func f(x : T): U { x }; // reject

func g(x : Int) : Nat { x }; // reject

func h(x : Int) : T { x }; // reject

func i(x : T) : Nat { x }; // reject

Prim.foo; // reject

do {
   (1,2,3,4,5,6,7,8,9,10,12,13,14,15,16,17,18,19,20) : (); // reject
};

do {
   (1,2,3,4,5,6,7,8,9,10,(12,13,14,(15,16,17,18,19,20))) : (); // reject
};

do {
   func p<T,U>(x : T, y : U) : (T,U) { (x,y) } ;
   let p1 = p(#A, #B);
   p1 : (); // reject
   let p2 = p(p1, p1);
   p2 : (); // reject
   let p3 = p(p2, p2);
   p3 : (); // reject
   let p4 = p(p3, p3);
   p4 : (); // reject
   let p5 = p(p4, p4);
   p5 : (); // reject
};

do {
   func p<T,U>(x:T, y:U) : {A : T; B : U } { {A = x; B = y} } ;
   let p1 = p(#A, #B);
   p1 : (); // reject
   let p2 = p(p1, p1);
   p2 : (); // reject
   let p3 = p(p2, p2);
   p3 : (); // reject
   let p4 = p(p3, p3);
   p4 : (); // reject
   let p5 = p(p4, p4);
   p5 : (); // reject
};

do {
   func p<T,U>(x:T, y:U) : module {A : T; B : U } { module { public let (A,B) = (x,y)} } ;
   let p1 = p(#A, #B);
   p1 : (); // reject
   let p2 = p(p1, p1);
   p2 : (); // reject
   let p3 = p(p2, p2);
   p3 : (); // reject
   let p4 = p(p3, p3);
   p4 : (); // reject
   let p5 = p(p4, p4);
   p5 : (); // reject
};

do {
   func p<T,U>(f : T->U) : (T->U)->(T->U) { func g {g}; } ;
   let p1 = p(func (#A) : {#B} {#B});
   p1 : (); // reject
   let p2 = p(p1);
   p2 : (); // reject
   let p3 = p(p2);
   p3 : (); // reject
   let p4 = p(p3);
   p4 : (); // reject
   let p5 = p(p4);
   p5 : (); // reject
};

do {
   func p<T,U>(f : <V>T->U) : <W>(<X>T->U)->(<Y>T->U) { func g {g}; } ;
   let p1 = p(func<Z>(#A) : {#B} {#B});
   p1 : (); // reject
   let p2 = p(p1);
   p2 : (); // reject
   let p3 = p(p2);
   p3 : (); // reject
   let p4 = p(p3);
   p4 : (); // reject
   let p5 = p(p4);
   p5 : (); // reject
};

do {
   func p<T>(x : T) : ?(T,T) { ?(x, x) } ;
   let p1 = p(1);
   p1 : (); // reject
   let p2 = p(p1);
   p2 : (); // reject
   let p3 = p(p2);
   p3 : (); // reject
   let p4 = p(p3);
   p4 : (); // reject
   let p5 = p(p4);
   p5 : (); // reject
};

do {
   func p<T,U>(x:T, y:U): {#A : T; #B : U } { #A x } ;
   let p1 = p(#Foo, #Bar);
   p1 : (); // reject
   let p2 = p(p1, p1);
   p2 : (); // reject
   let p3 = p(p2, p2);
   p3 : (); // reject
   let p4 = p(p3, p3);
   p4 : (); // reject
   let p5 = p(p4, p4);
   p5 : (); // reject
};
