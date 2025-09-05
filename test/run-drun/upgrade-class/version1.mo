import Test "test-class1";

persistent actor {
  var instance = Test.TestClass();
  instance.print();
};
