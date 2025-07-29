//MOC-FLAG --enhanced-orthogonal-persistence
import Prim "mo:⛔";
actor {

  type w = Weak Blob;  // type formation

  stable let w : Weak Blob = Prim.allocWeakRef("":Blob); // accept, stable with eop
  let wAlive = Prim.isLive(w);

  stable let bad1 : Weak (() -> ()) = Prim.allocWeakRef(func (){}); // reject,  non-stable

  let bad1Alive = Prim.isLive(bad1);

};


actor {

  public shared func bad2 (x: Weak Blob) : async (Weak Blob) { x }; // reject, not shared

}
