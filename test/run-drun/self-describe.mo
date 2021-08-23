import T "self-describe/types";
actor {
  public func hello(x : T.Rec) : async T.Foo {
    "Hello!"
  };
};
//CALL query __get_candid_interface_tmp_hack "DIDL\x00\x00"


//SKIP run
