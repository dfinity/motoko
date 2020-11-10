import Prim "mo:prim";
import M0 "class-import/empty";
import M1 "class-import/one";
import M2 "class-import/two";
import M3 "class-import/trap";

actor a {
 public func go() : async () {
   // test no arg class
   let empty : M0.Empty = await M0.Empty();
   await empty.test();

   // test single arg class
   let one : M1.One = await M1.One("one");
   await one.test();

   // test two arg class
   let two : M2.Two = await M2.Two("one","two");
   await two.test();

   // test non-trapping install
   try {
     let trap : M3.Trap = await M3.Trap(false);
   }
   catch _ {
     assert false;
   };

   // test trapping install
   try {
     let trap : M3.Trap = await M3.Trap(true);
     assert false;
   }
   catch _ {
     Prim.debugPrint("caught trap");
   };

 }
};

a.go() //OR-CALL ingress go "DIDL\x00\x00"
