import Prim "mo:prim";

persistent actor {
  class Wrapper(delegate: stable () -> ()) {
    public func call() {
      delegate();
    }
  };

  public func print() {
    Prim.debugPrint("Test");
  }

  transient let wrapper = Wrapper(print);

  stable let hiddenClosure = {
    message: Text;
  } = {
    message = "Hello";
    nonStable = wrapper.call;
  };

  type Record = {
    number: Int;
    nat64: Nat64;
    text: Text;
    blob: Blob;
    self: var Record;
  };

  stable let record = {
    number = -12345678901234567890123456789012345678901234567890123456789012345678901234567890;
    nat64 = 0xabcd_abcd_abcd_abcd;
    text = "Hello" # " " # "World";
    blob = "Text" : Blob;
    var self = null : ?Record;
  };

  record.self := record;

  stable var array = [var record; var record];
};


//SKIP run
//SKIP run-ir
//SKIP run-low

//CALL upgrade ""
