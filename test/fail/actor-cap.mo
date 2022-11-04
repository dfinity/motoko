import Prim "mo:⛔"

actor class C () = self {
  return self;
  throw (Prim.error("wrong"));
};

actor self {
   return self;
   throw (Prim.error("wrong"));
}
