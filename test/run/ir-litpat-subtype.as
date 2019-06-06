func bar (a : Nat) = switch a {
   case 25 ();
   case (25 : Int) ();   // OK: accepted by type checker, rejected by IR checker.
}