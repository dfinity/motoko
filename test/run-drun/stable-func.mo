import Prim "mo:prim";

actor {
   Prim.debugPrint("Version 0");

   stable func f() : () {
      Prim.debugPrint("f0");
   };

   f();
};
