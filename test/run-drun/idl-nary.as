actor {
  public func two(x:Text, y:Text) : async (Text, Text) {
    (x, y)
  };

  public func three(x:Text, y:Text, z: Text) : async (Text, Text, Text) {
    (x, y, z)
  };

  public func four(x:Text, y:Text, z: Text, w: Text) : async (Text, Text, Text, Text) {
    (x, y, z, w)
  };

  public func mkRecord() : async ((Text, Text, Text, Text)) {
    ("One", "Two", "Three", "Four")
  };

  public func unary1((x:Text, y:Text, z: Text, w: Text)) : async ((Text, Text, Text, Text)) {
    (x, y, z, w)
  };

  public func unary2(xyzw : (Text, Text, Text,Text)) : async ((Text, Text, Text, Text)) {
    xyzw
  };

  public func unary3(xyzw : (Text, Text, Text,Text)) : async ((Text, Text, Text, Text)) {
    xyzw
  };

  type T = (Text, Text, Text, Text);
  public func unary4(xyzw : (Text, Text, Text,Text)) : async T  {
    xyzw
  }


}

//CALL query two "DIDL\x00\x02\x71\x71\x03One\x03Two"
//CALL query three "DIDL\x00\x03\x71\x71\x71\x03One\x03Two\x05Three"
//CALL query four "DIDL\x00\x04\x71\x71\x71\x71\x03One\x03Two\x05Three\x04Four"
//CALL query mkRecord "DIDL\x00\x00"
//CALL query unary1 0x4449444c016c0400710171027103710100034f6e650354776f05546872656504466f7572
//CALL query unary2 0x4449444c016c0400710171027103710100034f6e650354776f05546872656504466f7572
//CALL query unary3 0x4449444c016c0400710171027103710100034f6e650354776f05546872656504466f7572
//CALL query unary4 0x4449444c016c0400710171027103710100034f6e650354776f05546872656504466f7572

