import Test "test-class";
import Prim "mo:prim";

persistent actor {
  var instance = Test.TestClass();
  instance.print();
  Prim.debugPrint("Version 1");
};
