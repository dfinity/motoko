type Exp = { #Lit : Nat;
             #Div : (Exp, Exp);
             #IfZero: (Exp, Exp, Exp)
           };

func evil( e: Exp) : ? Nat {
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
           evil e2 ! // not tail recursive
         else
           evil e3 ! // not tail recursive
       }
  })
}
