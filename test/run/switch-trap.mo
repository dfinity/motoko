import Prim "mo:⛔";

func byteFrom(seed : Blob) : Nat8 {
  switch (seed.vals().next()) {
    case (?w) { w };
    case _ { Prim.trap "Random.byteFrom" }
  }
};
byteFrom("foo");
