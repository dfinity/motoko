import Prim "mo:prim";
import Blob "blob.mo";
module {
  public func hash(x : PrincipalId) : Word32 = Blob.hash (Prim.blobOfPrincipalId x);
}
