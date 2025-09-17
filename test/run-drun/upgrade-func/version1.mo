import Prim "mo:prim";

actor {
   Prim.debugPrint("Version 1");

   stable func f() : () {
      Prim.debugPrint("f1");
   };

   stable let g = f;

   f();
   g();

};
