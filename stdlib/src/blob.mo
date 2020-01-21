import Prim "mo:prim";
module {
  public func hash(x : Blob) : Word32 = Prim.hashBlob x;
}
