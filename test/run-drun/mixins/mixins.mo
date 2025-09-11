import MyMixin "MyMixin";

persistent actor {
  include MyMixin("MyPrefix ", " MySuffix");

  let _x : MixinTy = 10;
  assert msg == "MyPrefix Hello from Mixin MySuffix";
  public func actorFunc() : async Text {
    assert (await mixinFunc()) == msg;
    "Hello from the actor"
  };

};
