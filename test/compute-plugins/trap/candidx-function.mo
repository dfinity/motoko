//MOC-FLAG --plugin ../../../_build/compute-plugins/candidx
import Prim "mo:⛔";

let service : actor { call : shared () -> async () } = actor "aaaaa-aa";
ignore Prim.to_candid_canonical(service.call);
