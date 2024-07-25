import { debugPrint; errorMessage; call_raw; principalOfActor; charToText; performanceCounter } "mo:⛔";

actor this {

  func hexdump(b : Blob) : Text{
    let s = debug_show b;
    var t = "";
    for (c in s.chars()) {
       if (c != '\\') { t #= charToText(c) }
    };
    t
  };

  public func vec_null_extra_argument() : async () {
    debugPrint("vec_null_extra_argument");
    debugPrint(debug_show {cycles = performanceCounter(0)});
  };

  public func vec_reserved_extra_argument() : async () {
    debugPrint("vec_reserved_extra_argument");
    debugPrint(debug_show {cycles = performanceCounter(0)});
  };

  public func zero_sized_record_extra_argument() : async () {
    debugPrint("zero_sized_record_extra_argument");
    debugPrint(debug_show {cycles = performanceCounter(0)});
  };

  public func vec_vec_null_extra_argument() : async () {
    debugPrint("vec_vec_null_extra_argument");
    debugPrint(debug_show {cycles = performanceCounter(0)});
  };

  public func vec_record_emp_extra_argument() : async () {
    debugPrint("vec_record_emp_extra_argument");
    debugPrint(debug_show {cycles = performanceCounter(0)});
  };

  public func vec_opt_record_with_2_20_null_extra_argument() : async () {
    debugPrint("vec_opt_record_with_2_20_null_extra_argument");
    debugPrint(debug_show {cycles = performanceCounter(0)});
  };

  public func vec_null_not_ignored(a: [?Nat]) : async () {
    debugPrint("vec_null_not_ignored");
    debugPrint(debug_show {cycles = performanceCounter(0)});
  };

  public func vec_reserved_not_ignored(a: [Any]) : async () {
    debugPrint("vec_reserved_not_ignored");
    debugPrint(debug_show {cycles = performanceCounter(0)});
  };

  // this test may be broken
  public func zero_sized_record_not_ignored(a: [{_0_: Null; _1_: {_0_:Any}; _2_: {}}]) : async () {
    debugPrint("zero_sized_record_not_ignored");
    debugPrint(debug_show {cycles = performanceCounter(0)});
  };

  public func vec_vec_null_not_ignored(a: [[Null]]) : async () {
    debugPrint("vec_vec_null_not_ignored");
    debugPrint(debug_show {cycles = performanceCounter(0)});
  };

  // this test may be broken
  public func vec_record_emp_not_ignored(a: [{}]) : async () {
    debugPrint("vec_record_emp_not_ignored");
     debugPrint(debug_show {cycles = performanceCounter(0)});
  };

  public func vec_null_subtyping(o: ?Nat) : async () {
    debugPrint("vec_null_subtyping");
    debugPrint(debug_show {cycles = performanceCounter(0)});
  };

  public func vec_reserved_subtyping(o: ?Nat) : async () {
    debugPrint("vec_reserved_subtyping");
    debugPrint(debug_show {cycles = performanceCounter(0)});
  };

  public func zero_sized_record_subtyping(o: ?Nat) : async () {
    debugPrint("vec_zero_sized_record_subtyping");
    debugPrint(debug_show {cycles = performanceCounter(0)});
  };

  public func vec_vec_null_subtyping(a: [?Nat]) : async () {
    debugPrint("vec_vec_null_subtyping");
    debugPrint(debug_show {cycles = performanceCounter(0)});
  };

  public func vec_record_emp_subtyping(a: ?Nat) : async () {
    debugPrint("vec_record_emp_subtyping");
    debugPrint(debug_show {cycles = performanceCounter(0)});
  };

  public func vec_opt_record_with_2_20_null_subtyping(a: [?{}]) : async () {
    debugPrint("vec_opt_record_with_2_20_null_subtyping");
    debugPrint(debug_show {cycles = performanceCounter(0)});
  };

  public func go() : async () {
    let p = principalOfActor(this);

    // Plain decoding (unused arguments)
    do {
      debugPrint("vec_null_extra_argument");
      try {
        ignore await call_raw(p, "vec_null_extra_argument","DIDL\01\6d\7f\01\00\80\94\eb\dc\03");
      }
      catch e {
        debugPrint( debug_show {vec_null_extra_argument = errorMessage(e)});
      }
    };

    do {
      debugPrint("vec_reserved_extra_argument");
      try {
        ignore await call_raw(p, "vec_reserved_extra_argument", "DIDL\01\6d\70\01\00\80\94\eb\dc\03" );
      }
      catch e {
        debugPrint( debug_show {vec_reserved_extra_argument = errorMessage(e)});
      }
    };

    do {
      debugPrint("zero_sized_record_extra_argument");
      try {
        ignore await call_raw(p, "zero_sized_record_extra_argument", "DIDL\04\6c\03\01\7f\02\01\03\02\6c\01\01\70\6c\00\6d\00\01\03\80\94\eb\dc\03");
      }
      catch e {
        debugPrint( debug_show {zero_sized_record_extra_argument = errorMessage(e)});
      }
    };


    do {
      debugPrint("vec_vec_null_extra_argument");
      let blobs = [
         "DIDL\02\6d\01\6d\7f\01\00\05\ff\ff\3f\ff\ff\3f\ff\ff\3f\ff\ff\3f\ff\ff\3f"
      ] : [Blob];
      for (blob in blobs.vals()) {
        // Decoding to actual type
        debugPrint("vec_vec_null_extra_argument");
        try {
          ignore await call_raw(p, "vec_vec_null_extra_argument", blob);
        }
        catch e {
          debugPrint( debug_show {vec_vec_null_extra_argument = errorMessage(e)});
        };
      }
    };

    do { // is this test broken?
      debugPrint("vec_record_emp_extra_argument");
      let blobs = [
        "DIDL\03\6c\01\d6\fc\a7\02\01\6d\02\6c\00\01\00\80\ad\e2\04"
      ] : [Blob];
      for (blob in blobs.vals()) {
        // Decoding to actual type
        debugPrint("vec_record_emp_extra_argument");
        try {
          ignore await call_raw(p, "vec_record_emp_extra_argument", blob);
        }
        catch e {
          debugPrint( debug_show {vec_record_emp_extra_argument = errorMessage(e); hex = hexdump(blob)});
        };
      }
    };

    do {
      debugPrint("vec_opt_record_with_2_20_null_extra_argument");
      let blobs = [
        "DIDL\17\6c\02\01\7f\02\7f\6c\02\01\00\02\00\6c\02\00\01\01\01\6c\02\00\02\01\02\6c\02\00\03\01\03\6c\02\00\04\01\04\6c\02\00\05\01\05\6c\02\00\06\01\06\6c\02\00\07\01\07\6c\02\00\08\01\08\6c\02\00\09\01\09\6c\02\00\0a\01\0a\6c\02\00\0b\01\0b\6c\02\00\0c\01\0c\6c\02\00\0d\02\0d\6c\02\00\0e\01\0e\6c\02\00\0f\01\0f\6c\02\00\10\01\10\6c\02\00\11\01\11\6c\02\00\12\01\12\6c\02\00\13\01\13\6e\14\6d\15\01\16\05\01\01\01\01\01"
      ] : [Blob];
      for (blob in blobs.vals()) {
        // Decoding to actual type
        debugPrint("vec_opt_record_with_2_20_null_extra_argument");
        try {
          ignore await call_raw(p, "vec_opt_record_with_2_20_null_extra_argument", blob);
        }
        catch e {
          debugPrint( debug_show {vec_opt_record_with_2_20_null_extra_argument = errorMessage(e); hex = hexdump(blob)});
        };
      }
    };

    // Decoding to actual type

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
          debugPrint( debug_show {vec_null_not_ignored = errorMessage(e); blob = hexdump(blob) });
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
    };


    do { // is this test broken?
      debugPrint("zero_sized_record_not_ignored");
      let blobs = [
         "DIDL\04\6c\03\01\7f\02\01\03\02\6c\01\01\70\6c\00\6d\00\01\03\80\94\eb\dc\03",
         "DIDL\04\6c\03\01\7f\02\01\03\02\6c\01\01\70\6c\00\6d\00\01\03\80\ad\e2\04",
         "DIDL\04\6c\03\01\7f\02\01\03\02\6c\01\01\70\6c\00\6d\00\01\03\ff\ff\3f",
         "DIDL\04\6c\03\01\7f\02\01\03\02\6c\01\01\70\6c\00\6d\00\01\03\80\b5\18",
      ] : [Blob];
      for (blob in blobs.vals()) {
        // Decoding to actual type
        debugPrint("vec_size_record_subtyping");
        try {
          ignore await call_raw(p, "zero_sized_record_not_ignored", blob);
        }
        catch e {
          debugPrint( debug_show {zero_sized_record_not_ignored = errorMessage(e); hex = hexdump(blob)});
        };
      }
    };

    do {
      debugPrint("vec_vec_null_not_ignored");
      let blobs = [
         "DIDL\02\6d\01\6d\7f\01\00\05\ff\ff\3f\ff\ff\3f\ff\ff\3f\ff\ff\3f\ff\ff\3f"
      ] : [Blob];
      for (blob in blobs.vals()) {
        // Decoding to actual type
        debugPrint("vec_vec_null_not_ignored");
        try {
          ignore await call_raw(p, "vec_vec_null_not_ignored", blob);
        }
        catch e {
          debugPrint( debug_show {vec_vec_null_not_ignored = errorMessage(e)});
        };
      }
    };

    do { // is this test broken?
      debugPrint("vec_record_emp_not_ignored");
      let blobs = [
        "DIDL\03\6c\01\d6\fc\a7\02\01\6d\02\6c\00\01\00\80\ad\e2\04"
      ] : [Blob];
      for (blob in blobs.vals()) {
        // Decoding to actual type
        debugPrint("vec_record_emp_not_ignored");
        try {
          ignore await call_raw(p, "vec_record_emp_not_ignored", blob);
        }
        catch e {
          debugPrint( debug_show {vec_record_emp_not_ignored = errorMessage(e); hex = hexdump(blob)});
        };
      }
    };

    // Decoding under opt
    do {
      debugPrint("vec_null_subtyping");
      // Decoding to actual type
      let blobs : [Blob] = [
        "DIDL\01\6d\7f\01\00\80\94\eb\dc\03" : Blob,
        "DIDL\01\6d\7f\01\00\80\ad\e2\04" : Blob,
        "DIDL\01\6d\7f\01\00\ff\ff\3f" : Blob,
        "DIDL\01\6d\7f\01\00\80\bf\18" : Blob,
      ];
      for (blob in blobs.vals()) {
        debugPrint(debug_show { vec_null_subtyping = blob } );
        try {
          ignore await call_raw(p, "vec_null_subtyping", blob);
          debugPrint( debug_show {vec_null_subtyping = "ok"; blob = hexdump(blob) });
        }
        catch e {
          debugPrint( debug_show {vec_null_subtyping = errorMessage(e); blob = hexdump(blob) });
        };
      }
    };

    do {
      debugPrint("vec_reserved_subtyping");
      // Decoding to actual type
      let blobs : [Blob] = [
        "DIDL\01\6d\70\01\00\80\94\eb\dc\03" : Blob,
        "DIDL\01\6d\70\01\00\80\ad\e2\04" : Blob,
        "DIDL\01\6d\70\01\00\ff\ff\3f" : Blob,
        "DIDL\01\6d\70\01\00\80\bf\18" : Blob,
      ];
      for (blob in blobs.vals()) {
        debugPrint(debug_show { vec_reserved_subtyping = blob } );
        try {
          ignore await call_raw(p, "vec_reserved_subtyping", blob);
          debugPrint( debug_show {vec_reserved_subtyping = "ok"; blob = hexdump(blob) });
        }
        catch e {
          debugPrint( debug_show {vec_reservd_subtyping = errorMessage(e); blob = hexdump(blob) });
        };
      }
    };

    do { // is this test broken?
      debugPrint("zero_sized_record_subtyping");
      let blobs = [
         "DIDL\04\6c\03\01\7f\02\01\03\02\6c\01\01\70\6c\00\6d\00\01\03\80\94\eb\dc\03",
         "DIDL\04\6c\03\01\7f\02\01\03\02\6c\01\01\70\6c\00\6d\00\01\03\80\ad\e2\04",
         "DIDL\04\6c\03\01\7f\02\01\03\02\6c\01\01\70\6c\00\6d\00\01\03\ff\ff\3f",
         "DIDL\04\6c\03\01\7f\02\01\03\02\6c\01\01\70\6c\00\6d\00\01\03\80\b5\18",
      ] : [Blob];
      for (blob in blobs.vals()) {
        // Decoding to actual type
        debugPrint("zero_size_record_subtyping");
        try {
          ignore await call_raw(p, "zero_sized_record_subtyping", blob);
        }
        catch e {
          debugPrint( debug_show {zero_sized_record_subtyping = errorMessage(e); hex = hexdump(blob)});
        };
      }
    };

    do {
      debugPrint("vec_vec_null_subtyping");
      let blobs = [
         "DIDL\02\6d\01\6d\7f\01\00\05\ff\ff\3f\ff\ff\3f\ff\ff\3f\ff\ff\3f\ff\ff\3f"
      ] : [Blob];
      for (blob in blobs.vals()) {
        // Decoding to actual type
        debugPrint("vec_vec_null_subtyping");
        try {
          ignore await call_raw(p, "vec_vec_null_subtyping", blob);
        }
        catch e {
          debugPrint( debug_show {vec_vec_null_subtyping = errorMessage(e)});
        };
      }
    };


    do { // is this test broken?
      debugPrint("vec_record_emp_subtyping");
      let blobs = [
        "DIDL\03\6c\01\d6\fc\a7\02\01\6d\02\6c\00\01\00\80\ad\e2\04"
      ] : [Blob];
      for (blob in blobs.vals()) {
        // Decoding to actual type
        debugPrint("vec_record_emp_subptying");
        try {
          ignore await call_raw(p, "vec_record_emp_subtyping", blob);
        }
        catch e {
          debugPrint( debug_show {vec_record_emp_subtyping = errorMessage(e); hex = hexdump(blob)});
        };
      }
    };

    do {
      debugPrint("vec_opt_record_with_2_20_null_subtyping");
      let blobs = [
        "DIDL\17\6c\02\01\7f\02\7f\6c\02\01\00\02\00\6c\02\00\01\01\01\6c\02\00\02\01\02\6c\02\00\03\01\03\6c\02\00\04\01\04\6c\02\00\05\01\05\6c\02\00\06\01\06\6c\02\00\07\01\07\6c\02\00\08\01\08\6c\02\00\09\01\09\6c\02\00\0a\01\0a\6c\02\00\0b\01\0b\6c\02\00\0c\01\0c\6c\02\00\0d\02\0d\6c\02\00\0e\01\0e\6c\02\00\0f\01\0f\6c\02\00\10\01\10\6c\02\00\11\01\11\6c\02\00\12\01\12\6c\02\00\13\01\13\6e\14\6d\15\01\16\05\01\01\01\01\01"
      ] : [Blob];
      for (blob in blobs.vals()) {
        // Decoding to actual type
        debugPrint("vec_opt_record_with_2_20_null_subtyping");
        try {
          ignore await call_raw(p, "vec_opt_record_with_2_20_null_subtyping", blob);
        }
        catch e {
          debugPrint( debug_show {vec_opt_record_with_2_20_null_subtyping = errorMessage(e); hex = hexdump(blob)});
        };
      }
    };


    // TBC from spacebomb.test.did
}

}
//CALL ingress go "DIDL\x00\x00"
//SKIP run
//SKIP run-ir
//SKIP run-low
