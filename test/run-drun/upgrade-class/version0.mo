import Test "test-class0";

persistent actor {
  var instance = Test.TestClass();
  instance.print();
};
