import imported "actor.did";
actor a {
  public func go() : async Int8 = async await imported.h(42);
  public func o() : async imported.o = async await imported.o(null);
};

