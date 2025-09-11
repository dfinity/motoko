import MyMixin "MyMixin";

persistent actor {
  include MyMixin("MyPrefix ", " MySuffix");

  assert msg == "MyPrefix Hello from Mixin MySuffix";
  public query func actorFunc() : async Text {
    "Hello from the actor"
  };

};
