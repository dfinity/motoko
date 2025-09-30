import Prim "mo:prim";

persistent actor {

  public func test(b : Blob) : async () {
    Prim.debugPrint(debug_show (b));
  };

  // WeakRef type.
  type WeakRef = {
    ref : weak Blob;
  };
  // A linked list of WeakRefs.
  type List = {
    next : ?List;
    value : ?WeakRef;
    originalBlob : Blob;
    index : Nat;
  };
  func getHashArrayLen(hashArray : [var List]) : Nat {
    var len = 0;
    var i = 0;
    while (i < 16_384) {
      len += hashArray[i].index;
      i += 1;
    };
    len;
  };
  func showMeAllBlobs(hashArray : [var List]) : () {
    var i = 0;
    while (i < 16_384) {
      // Deref the weak ref.
      let weakRef = hashArray[i].value;
      switch weakRef {
        case (?weakRef) {
          Prim.debugPrint(debug_show (hashArray[i].originalBlob));
          Prim.debugPrint(debug_show ("============"));
          Prim.debugPrint(debug_show (weakRef.ref));
        };
        case null {};
      };
      i += 1;
    };
  };

  public func test2() : async () {

    let hash = Prim.__getDedupTable();
    switch hash {
      case (?hashArray) {
        Prim.debugPrint(debug_show (getHashArrayLen(hashArray)));
        assert (getHashArrayLen(hashArray) == 6);
        //showMeAllBlobs(hashArray);
      };
      case null {};
    };

  };

};

//SKIP run
//SKIP run-ir
//SKIP run-low
//ENHANCED-ORTHOGONAL-PERSISTENCE-ONLY
