actor {
  public query func two(x:Text, y:Text) : async (Text, Text) {
    (x, y)
  };

  public query func three(x:Text, y:Text, z: Text) : async (Text, Text, Text) {
    (x, y, z)
  };

  public func four(x:Text, y:Text, z: Text, w: Text) : async (Text, Text, Text, Text) {
    (x, y, z, w)
  };

  public query func mkRecord() : async ((Text, Text, Text, Text)) {
    ("One", "Two", "Three", "Four")
  };

  public query func unary1((x:Text, y:Text, z: Text, w: Text)) : async ((Text, Text, Text, Text)) {
    (x, y, z, w)
  };

  public func unary2(xyzw : (Text, Text, Text, Text)) : async ((Text, Text, Text, Text)) {
    xyzw
  };

  public query func unary3(xyzw : (Text, Text, Text, Text)) : async ((Text, Text, Text, Text)) {
    xyzw
  };

  type T = (Text, Text, Text, Text);
  public func unary4(xyzw : (Text, Text, Text, Text)) : async T  {
    xyzw
  }


}

//CALL ingress two "DIDL\x00\x02\x71\x71\x03One\x03Two"
//CALL ingress three "DIDL\x00\x03\x71\x71\x71\x03One\x03Two\x05Three"
//CALL ingress four "DIDL\x00\x04\x71\x71\x71\x71\x03One\x03Two\x05Three\x04Four"
//CALL ingress mkRecord "DIDL\x00\x00"
//CALL ingress unary1 0x4449444c016c0400710171027103710100034f6e650354776f05546872656504466f7572
//CALL ingress unary2 0x4449444c016c0400710171027103710100034f6e650354776f05546872656504466f7572
//CALL ingress unary3 0x4449444c016c0400710171027103710100034f6e650354776f05546872656504466f7572
//CALL ingress unary4 0x4449444c016c0400710171027103710100034f6e650354776f05546872656504466f7572


