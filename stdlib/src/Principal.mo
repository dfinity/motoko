/**
[#mod-Principal]
= `Principal` -- IC principals (User and canister IDs)
*/

import Prim "mo:prim";
import Blob "Blob";
module {
  public let hash : Principal -> Word32 =
    func(x) = Blob.hash (Prim.blobOfPrincipal x);

  public let fromActor : (actor {}) -> Principal =
    Prim.principalOfActor;
}
