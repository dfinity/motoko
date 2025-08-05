import Prim "mo:prim";

actor {
   Prim.debugPrint("Version 2");

   stable func f() : () {
      Prim.debugPrint("f2");
   };

   stable let g = f;

   f();
   g();

};
