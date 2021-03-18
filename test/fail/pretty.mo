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
         


func f(x : T): U { x }   ;

func g(x : Int) : Nat { x };


func h(x : Int) : T { x };