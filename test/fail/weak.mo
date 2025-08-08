//MOC-FLAG --enhanced-orthogonal-persistence
import Prim "mo:⛔";
actor {

  type w = weak Blob; // type formation

  stable let w : weak Blob = Prim.allocWeakRef("" : Blob); // accept, stable with eop
  let wAlive = Prim.isLive(w);

  stable let bad1 : weak(() -> ()) = Prim.allocWeakRef(func() {}); // reject,  non-stable

  let bad1Alive = Prim.isLive(bad1);

};

actor {

  public shared func bad2(x : weak Blob) : async (weak Blob) { x }; // reject, not shared

};
