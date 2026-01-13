class migration({}) = {};

(with migration; moot = "MOOT")
actor {
  func a() : async () {
    await ({ cycles = -3 } with) async ();
    await ({ cycles = "" } with) async ();
    await (with cycles = "") async ();
    func cycles() {};
    await (with cycles) async ();
    await (with timeout = "") async ();
  };
  func b() : async () {
    object base {
      public func cycles() {}
    };
    await (base with) async ();
  };
};
