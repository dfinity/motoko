//MOC-FLAG -dl --enhanced-orthogonal-persistence
import Region "issue-5767/Region";

actor A {

  let r = Region.new();
  let _ = Region.grow(r, 1);

  func kermit() {

    let w1 = r.loadNat64(0);
    let w2 = Region.loadNat64(r, 0);
    let w3 = Region.loadNat64Fn(r, 0);

  };

  public func go() : async () {

    kermit();

  }

};

//(); //OR-CALL ingress go "DIDL\x00\x00"

