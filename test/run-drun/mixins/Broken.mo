mixin() {
  public func pub() : async () {};
  private let ok = pub; // fine?
  public let rebound = pub; // rejected

  private shared func f() : async () {}; // rejected
};
