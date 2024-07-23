import { debugPrint; errorMessage; call_raw; principalOfActor } "mo:⛔";

actor this {

  public func vec_null_extra_argument() : async () {
    debugPrint("vec_null_extra_argument");
  };

  public func vec_reserved_extra_argument() : async () {
    debugPrint("vec_reserved_extra_argument");
  };

  public func zero_sized_record_extra_argument() : async () {
    debugPrint("zero_sized_record_extra_argument");
  };

  public func vec_null_not_ignored(a:[?Nat]) : async () {
    debugPrint("vec_null_not_ignored");
  };

  public func vec_reserved_not_ignored(a:[Any]) : async () {
    debugPrint("vec_reserved_not_ignored");
  };


  public func go() : async () {
    let p = principalOfActor(this);

    // Plain decoding (unused arguments)
    debugPrint("vec_null_extra_argument");
    try {
      ignore await call_raw(p, "vec_null_extra_argument","DIDL\01\6d\7f\01\00\80\94\eb\dc\03");
    }
    catch e {
      debugPrint( debug_show {vec_null_extra_argument = errorMessage(e)});
    };

    debugPrint("vec_reserved_extra_argument");
    try {
      ignore await call_raw(p, "vec_reserved_extra_argument", "DIDL\01\6d\70\01\00\80\94\eb\dc\03");
    }
    catch e {
      debugPrint( debug_show {vec_reserved_extra_argument = errorMessage(e)});
    };

    debugPrint("zero_sized_record_extra_argument");
    try {
      ignore await call_raw(p, "zero_sized_record_extra_argument", "DIDL\04\6c\03\01\7f\02\01\03\02\6c\01\01\70\6c\00\6d\00\01\03\80\94\eb\dc\03");
    }
    catch e {
      debugPrint( debug_show {zero_sized_record_extra_argument = errorMessage(e)});
    };

    // TBC from spacebomb.test.did

    do {
    debugPrint("vec_null_not_ignored");
    // Decoding to actual type
    let blobs : [Blob] = [
      "DIDL\01\6d\7f\01\00\80\94\eb\dc\03" : Blob,
      "DIDL\01\6d\7f\01\00\80\ad\e2\04" : Blob,
      "DIDL\01\6d\7f\01\00\ff\ff\3f" : Blob,
      "DIDL\01\6d\7f\01\00\80\bf\18" : Blob,
    ];
    for (blob in blobs.vals()) {
      debugPrint(debug_show { vec_null_not_ignored = blob } );
      try {
        ignore await call_raw(p, "vec_null_not_ignored", blob);
      }
      catch e {
        debugPrint( debug_show {vec_null_not_ignored = errorMessage(e)});
      };
    }
    };

    do {
    debugPrint("vec_reserved_not_ignored");
    let blobs : [Blob] = [
       "DIDL\01\6d\70\01\00\80\94\eb\dc\03" : Blob,
       "DIDL\01\6d\70\01\00\80\ad\e2\04" : Blob,
       "DIDL\01\6d\70\01\00\ff\ff\3f" : Blob,
       "DIDL\01\6d\70\01\00\80\bf\18" : Blob,
     ];
     for (blob in blobs.vals()) {
     // Decoding to actual type
     debugPrint("vec_reserved_not_ignored");
     try {
       ignore await call_raw(p, "vec_reserved_not_ignored", blob);
     }
     catch e {
      debugPrint( debug_show {vec_reserved_not_ignored = errorMessage(e)});
     };
     }
     }

     // TBC from spacebomb.test.did
}

}
//CALL ingress go "DIDL\x00\x00"
//SKIP run
//SKIP run-ir
//SKIP run-low
