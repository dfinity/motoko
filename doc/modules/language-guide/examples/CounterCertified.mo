/// Simple counter (see `Counter.mo`), but uses `mo:base/CertifiedData` to
/// implement the counter value as a certified variable.
import CD "mo:base/CertifiedData";
import Blob "mo:base/Blob";

actor Counter {

  var value : Nat32 = 0;

  /// Helper; should be in base?
  func blobOfNat64(n : Nat32) : Blob { 
    let byteMask : Nat32 = 0xff;
    Blob.fromArray(
      [byteMask << 0 & value,
       byteMask << 8 & value,
       byteMask << 16 & value,
       byteMask << 24 & value])    
  };

  /// Update counter and certificate (via system).
  public func inc() : async Nat64 {
    value += 1;
    CD.set(value);
    return value; 
  };
  
  /// Returns the current counter value,
  /// and an unforgeable certificate (from the system) about its authenticity.
  public query func get() : async ?{ value : Nat64; certificate : Blob } {
    do ? {
      (value, CD.getCertificate()!)
    }
  };
}
