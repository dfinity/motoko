// This tests checks that the IDL decoder properly
// zooms past the any argument and finds the beginning of the string
actor {
  public func any(_ : Any, x : Text) : async () {
     Debug.print ("ok: " # x);
  };
}

// Primitive values:
//CALL ingress any 0x4449444C00027f7103466F6F
//CALL ingress any 0x4449444C00027e710103466F6F
//CALL ingress any 0x4449444C00027d718080800803466F6F
//CALL ingress any 0x4449444C00027c718080800803466F6F
//CALL ingress any 0x4449444C00027b71AB03466F6F
//CALL ingress any 0x4449444C00027a71ABCD03466F6F
//CALL ingress any 0x4449444C00027971DEADBEEF03466F6F
//CALL ingress any 0x4449444C00027871DEADBEEFCAFFEE6603466F6F
//CALL ingress any 0x4449444C00027771AB03466F6F
//CALL ingress any 0x4449444C00027671ABCD03466F6F
//CALL ingress any 0x4449444C00027571DEADBEEF03466F6F
//CALL ingress any 0x4449444C00027471DEADBEEFCAFFEE6603466F6F
//CALL ingress any 0x4449444C00027371DEADBEEF03466F6F
//CALL ingress any 0x4449444C00027271DEADBEEFCAFFEE6603466F6F
//CALL ingress any 0x4449444C0002717103466F6F03466F6F
//CALL ingress any 0x4449444C0002707103466F6F

// Composite values:
//CALL ingress any 0x4449444C016e710200710003466F6F
//CALL ingress any 0x4449444C016e710200710103466F6F03466F6F
//CALL ingress any 0x4449444C016d710200710003466F6F
//CALL ingress any 0x4449444C016d710200710103466F6F03466F6F
//CALL ingress any 0x4449444C016d71020071020003466F6F03466F6F
//CALL ingress any 0x4449444C016c0002007103466F6F
//CALL ingress any 0x4449444C016c01800175020071DEADBEEF03466F6F
//CALL ingress any 0x4449444C016c02007180017502007101A0DEADBEEF03466F6F
//CALL ingress any 0x4449444C016b0180017502007100DEADBEEF03466F6F
//CALL ingress any 0x4449444C016b0200718001750200710001A003466F6F
//CALL ingress any 0x4449444C016b02007180017502007101DEADBEEF03466F6F
//CALL ingress any 0x4449444C016602ABCD0200710400DEADBEEF03466F6F
