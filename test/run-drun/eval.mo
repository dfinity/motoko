
type Exp = { #Lit : Nat;
             #Div : (Exp, Exp);
             #IfZero: (Exp, Exp, Exp)
           };

/*
do {
func evil( e: Exp) : ? Nat =
  ? (switch e {
       case (#Lit(n)) { n };
       case (#Div (e1, e2)) {
         let v1 = evil e1 !;
         let v2 = evil e2 !;
         if (v2 == 0)
           null ! // silly bang
         else v1 / v2;
       };
       case (#IfZero (e1, e2, e3)) {
         if (evil e1 ! == 0)
           evil e2 !  // not tail recursive
         else
           evil e3 !  // not tail recursive
       }
   });
}
*/

func eval( e: Exp) : ? Nat {
  do ? {
    switch e {
       case (#Lit(n)) { ? n };
       case (#Div (e1, e2)) {
         let v1 = eval e1 !;
         let v2 = eval e2 !;
         if (v2 == 0)
           null // no silly bang
         else ? (v1 / v2);
       };
       case (#IfZero (e1, e2, e3)) {
         if (eval e1 ! == 0)
           eval e2   // tail recursive
         else
           eval e3  // tail recursive
       }
    }
  }
}

/*
func eval( e: Exp) : Result<Nat,Text> {
  // label "!" { ... }
  do #Ok {
    // e ! =def= let v = e in case v of #Ok _ -> v | other -> break "!" other;
    switch e {
       case (#Lit(n)) { #Ok n };
       case (#Div (e1, e2)) {
         let v1 = eval e1 !;
         let v2 = eval e2 !;
         if (v2 == 0)
           #Err "DIV/0"
         else #Ok (v1 / v2);
       };
       case (#IfZero (e1, e2, e3)) {
         if (eval e1 ! == 0)
           eval e2
         else
           eval e3
       }
    }
  }
}
*/
