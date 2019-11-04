// This test checks that the IDL decoder can
// do the subtyping from received many-field record
// to a double-field one (field names are in hash order)
actor {
    public func pair(o : (Text, Int)) : async () {
     switch o {
       case (content, num) debug_print ("ok: " # debug_show num);
     }
  };
  public func record(o : {content: Text; value : Int}) : async () {
     switch o {
       case {content} debug_print ("ok: " # content);
     }
  };
  public func record1(o : {value : Int; byte : Int8}) : async () {
     switch o {
       case {byte} debug_print ("ok: " # debug_show byte);
     }
  };
    public func record2(o : {content: Text; value : Int}, follower : Int8) : async Int8 {
     switch o {
       case {content} { debug_print ("ok: " # " " # content # " " # debug_show follower); follower };
     }
  };
}

//CALL ingress pair 0x4449444C016C020071017C010004486579212A
//CALL ingress record 0x4449444C016C02b99adecb0171f1fee18d037C010004486579212A
// SKIPPED
// CALL ingress record1 0x4449444C016C0388be8c890477b99adecb0171f1fee18d037C010004486579212A19
//  needs to jump over redundant `content` field
//CALL ingress record1 0x4449444C016C03b99adecb0171f1fee18d037C88be8c890477010004486579212A19
//  needs to jump over redundant trailing `byte` field
//CALL ingress record2 0x4449444C016C03b99adecb0171f1fee18d037C88be8c89047702007704486579212A1819
