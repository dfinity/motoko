actor {
  public func hello(who : Text) : async Text {
    "Hello " # who # "!";
  }
}

//CALL query hello "DIDL\x00\x01\x71\x05World"
