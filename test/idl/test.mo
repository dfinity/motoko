import imported "actor.did";
actor a {
  public func go() : async (actor {}) = async imported;
};

