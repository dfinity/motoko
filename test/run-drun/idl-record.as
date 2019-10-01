// This test checks that the IDL decoder can
// do the subtyping from received many-field record to
// a double-field one
actor {
    public func pair(o : (Text, Int)) : async () {
     switch o {
       case (content, num) print ("ok: " # debug_show num);
     }
  };
  public func record(o : {content: Text; value : Int}) : async () {
     switch o {
       case {content} print ("ok: " # content);
     }
  };
}

//CALL ingress pair 0x4449444C016C020071017C010004486579212A
//CALL ingress record 0x4449444C016C02b99adecb0171f1fee18d037C010004486579212A
