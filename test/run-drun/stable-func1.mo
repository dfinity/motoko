import Prim "mo:prim";

actor {
   Prim.debugPrint("Version 0");

   stable func f() : () {
      Prim.debugPrint("f0");
   };

   stable let g : stable f () -> () = f;

   f();
   g();

};
