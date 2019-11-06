actor {
  public query func hello(who : Text) : future Text {
    "Hello " # who # "!";
  };
  public query func hello2(who : Text) : future Text {
    return ("Hello " # who # "!");
  }
}

//CALL query hello "DIDL\x00\x01\x71\x05World"
//CALL query hello2 "DIDL\x00\x01\x71\x05World"
