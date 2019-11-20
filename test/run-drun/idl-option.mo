// This test checks that the IDL decoder can
// do the subtyping from null to option
actor {
  public func any(o : ?Text) : async () {
     switch o {
       case null debugPrint ("ok: null");
       case (?x) debugPrint ("ok: " # x);
     }
  };
}

//CALL ingress any 0x4449444C00017f
//CALL ingress any 0x4449444C016e71010000
//CALL ingress any 0x4449444C016e7101000103466F6F
