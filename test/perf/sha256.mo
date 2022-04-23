import SHA256 "./sha256/SHA256";
import Array "./sha256/Array";
import Nat8 "./sha256/Nat8";
actor {
  public func go() : async () {
    // 256kb already exceed the instruction limit
    ignore SHA256.sha256(Array.tabulate(128*1024, Nat8.fromIntWrap));
  }
}
//CALL ingress go 0x4449444C0000
