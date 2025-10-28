import Result "mo:core/Result";

module zstd {
  public func encodeAll(data : Blob, level : Int32) : Result.Result<Blob, Text> =
      ((func "component:zstd:encode-all") : (Blob, Int32)  -> Result.Result<Blob, Text>) (data, level);
  public func decodeAll(data : Blob) : Result.Result<Blob, Text> =
      ((func "component:zstd:decode-all") : (Blob)  -> Result.Result<Blob, Text>) (data);
};
