type Foo = Text;
type Rec = { foo : Rec };
actor {
  public func hello(x : Rec) : async Foo {
    "Hello!"
  };
};
//CALL query __get_candid_interface_tmp_hack "DIDL\x00\x00"


//SKIP run
