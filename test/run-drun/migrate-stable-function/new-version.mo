import Prim "mo:prim";

persistent actor {
  class Wrapper(delegate : stable () -> ()) {
    public func call() {
      delegate();
    };
  };

  func print() {
    Prim.debugPrint("Test");
  };

  transient let wrapper = Wrapper(print);

  transient let transientObject = {
    message = "Hello";
    nonStable = wrapper.call;
    var nonStableVar = wrapper.call;
  };

  stable let hiddenClosure : {
    message : Text;
  } = transientObject;

  type Record = {
    number : Int;
    nat64 : Nat64;
    text : Text;
    blob : Blob;
    var self : ?Record;
  };

  stable let record = {
    number = -12345678901234567890123456789012345678901234567890123456789012345678901234567890;
    nat64 = 0xabcd_abcd_abcd_abcd : Nat64;
    text = "Hello" # " " # "World";
    blob = "Text" : Blob;
    var self = null : ?Record;
  };

  record.self := ?record;

  stable var array = [var record, record];

  func unwrap<T>(option : ?T) : T {
    switch (option) {
      case null Prim.trap("null trap");
      case (?value) value;
    };
  };

  Prim.debugPrint(debug_show(hiddenClosure));
  Prim.debugPrint(debug_show (unwrap(unwrap(array[0].self).self).number));
  Prim.debugPrint(debug_show (array[0].text));
};
