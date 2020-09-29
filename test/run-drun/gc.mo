import Prim "mo:prim";
// check blob and text iterator objects survice gc correctly
actor {
  public shared func testBytes() : async () {
    let blob = "\00\01\02\03": Blob;
    for (b in blob.bytes()) {
      await async {};
      Prim.debugPrint(debug_show b);
      return ()
    };
  };

  public shared func testChars() : async () {
    let text = "abcd" # "efgh" # "hijklmnopqrstuvwxyz";
    for (c in text.chars()) {
      await async {};
      Prim.debugPrint(debug_show c);
      return ()
    };
  };

}

//CALL ingress testBytes "DIDL\x00\x00"
///CALL ingress testChars "DIDL\x00\x00"

