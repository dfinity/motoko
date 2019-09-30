actor {
  public func rec({foo = (x:Text)}) : async () {
     print ("ok: " # x);
  };

  public func tell_me_the_idl_foo() : async {foo : Text} {
     { foo = "" }
  };
  public func tell_me_the_idl_bar() : async {bar : Text} {
     { bar = "" }
  };
}

// IDL hash for "foo" is 868eb702 in LEB128
// IDL hash for "bar" is d3e3aa02 in LEB128

//CALL ingress tell_me_the_idl_foo 0x4449444C0000
//CALL ingress tell_me_the_idl_bar 0x4449444C0000


