//MOC-FLAG --plugin ../../../_build/compute-plugins/sha256 --plugin ../../../_build/compute-plugins/candidx
import Prim "mo:⛔";

let digest = Prim.sha256("abc");
assert Prim.to_candid_canonical(digest) ==
  Prim.to_candid_canonical(Prim.blobToArray(digest));
