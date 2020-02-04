import Prim "mo:prim";
import Blob "blob";
module {
  public func hash(x : Principal) : Word32 = Blob.hash (Prim.blobOfPrincipal x);
}
