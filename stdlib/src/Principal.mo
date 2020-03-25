/**
[#mod-Principal]
= `Principal` -- IC principals (User and canister IDs)
*/

import Prim "mo:prim";
import Blob "Blob";
module {
  public func hash(x : Principal) : Word32 = Blob.hash (Prim.blobOfPrincipal x);
}
