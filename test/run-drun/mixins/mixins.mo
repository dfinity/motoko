import MyMixin "MyMixin";

persistent actor {
  include MyMixin();

  let myMsg = msg;
  public query func actorFunc() : async Text {
    "Hello from the actor"
  };

};
