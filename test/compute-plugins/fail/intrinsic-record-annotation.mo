//MOC-FLAG --plugin ../../../_build/compute-plugins/candidx
import Prim "mo:⛔";

let inferred = { encode = Prim.to_candid_canonical };
let _ : { encode : () -> Blob } = inferred;
