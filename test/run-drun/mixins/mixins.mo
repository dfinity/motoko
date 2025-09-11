import MyMixin "MyMixin";

persistent actor {
  public query func actorFunc() : async Text {
    "Actor";
  };
  include MyMixin();
};
