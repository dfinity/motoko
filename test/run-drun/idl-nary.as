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

  public func mkRecord() : async ((Text, Text, Text, Text),) {
    (("One", "Two", "Three", "Four"),)
  };

  public func unary((x:Text, y:Text, z: Text, w: Text),) : async ((Text, Text, Text, Text),) {
    ((x, y, z, w),)
  }


}

//CALL query two "DIDL\x00\x02\x71\x71\x03One\x03Two"
//CALL query three "DIDL\x00\x03\x71\x71\x71\x03One\x03Two\x05Three"
//CALL query four "DIDL\x00\x04\x71\x71\x71\x71\x03One\x03Two\x05Three\x04Four"
//CALL query mkRecord "DIDL\x00\x00"
//CALL query unary 0x4449444c016c0400710171027103710100034f6e650354776f05546872656504466f7572

