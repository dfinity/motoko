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
