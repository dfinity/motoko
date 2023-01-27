// This test exists to verify the fix for
// https://dfinity.atlassian.net/browse/EXC-1318
// drun didn't implement raw_rand properly, returning the same bytes each time.
// Once this test starts failing (correctly) due to a new drun, change the assert on the last line (or enable it in rand.mo and delete this file)
// and let Dragoljub Duric at DFINITY know!
import Prim "mo:⛔";

actor a {

  let rand = (actor "aaaaa-aa" : actor { raw_rand : () -> async Blob }).raw_rand;

  public func go() : async () {
    let b1 = await rand();
    let b2 = await rand();
    assert b1.size() == 32;
    assert b2.size() == 32;
    assert b1 == b2 // should fail, but doesn't on drun!
  };
};
//SKIP run
//SKIP run-ir
//SKIP run-low
//SKIP ic-ref-run

//CALL ingress go "DIDL\x00\x00"
