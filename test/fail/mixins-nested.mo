import Nested "mixins/Nested";
persistent actor {
  include Nested();
  public func test() : async () {
    await increment(); // transitive include
    await decrement();
  };
};
