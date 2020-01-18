import Prim "mo:prim";
import Blob "blob.mo";
module {
  public func hash(x : Principal) : Word32 = Blob.hash (Prim.blobOfPrincipal x);
}
