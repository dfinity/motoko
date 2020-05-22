import Prim "mo:prim"

actor class C () = self {
  return self;
  throw (Prim.error("wrong"));
};

actor self {
   return self;
   throw (Prim.error("wrong"));
}
