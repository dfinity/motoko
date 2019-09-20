actor {
  public func hello(who : Text) : async Text {
    "Hello " # who # "!";
  };
  public func hello2(who : Text) : async Text {
    return ("Hello " # who # "!");
  }
}

//CALL query hello "DIDL\x00\x01\x71\x05World"
//CALL query hello2 "DIDL\x00\x01\x71\x05World"
