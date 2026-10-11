//MOC-FLAG --plugin ../../../_build/compute-plugins/candidx
//MOC-FLAG --plugin ../../../_build/compute-plugins/sha256
//MOC-FLAG --sanity-checks --force-gc
//SKIP run
//SKIP run-ir
//SKIP run-low

import Prim "mo:⛔";

actor {
  type Entry = {
    index : Nat;
    payload : Blob;
    memo : Text;
    metadata : ?{ tags : [Text]; state : { #ready; #seen : Nat } };
  };
  type Saved = { original : Entry; outputs : [Blob] };
  type Group = { entries : [Saved] };
  type Fixed = { index : Nat64; payload : Blob };
  type Choice = { #ready; #seen : Nat };

  var retained : [Group] = [];
  var fixed : [Fixed] = [];
  var choices : [Choice] = [];
  var fixedOutput : Blob = "";
  var choicesOutput : Blob = "";
  var large : Blob = "";
  var generation = 0;
  var reclaimedBefore = 0;

  func verify() {
    assert retained.size() == 2;
    assert Prim.to_candid_canonical(fixed) == fixedOutput;
    assert Prim.to_candid_canonical(choices) == choicesOutput;
    assert fixed[63].index == Prim.natToNat64(generation + 63);
    assert choices[63] == #seen(generation + 63);
    for (group in retained.values()) {
      for (saved in group.entries.values()) {
        let encoded = saved.outputs[0];
        assert Prim.to_candid_canonical(saved.original) == encoded;
        assert Prim.sha256(encoded) == saved.outputs[1];
        let decoded : ?Entry = from_candid encoded;
        assert decoded == ?saved.original;
      };
    };
    assert Prim.sha256(large) ==
      "\65\d8\0b\2b\0f\01\20\bd\a1\62\8b\37\49\98\20\90\e0\58\d2\d9\f4\a5\fa\d1\bc\70\25\0b\49\bc\4e\96";

    // Frozen wire bytes and a known SHA-256 answer also check the two plugins
    // independently of the results retained before collection.
    let abc = Prim.arrayToBlob([97, 98, 99]);
    assert Prim.to_candid_canonical(abc) == "DIDL\01\6d\7b\01\00\03abc";
    assert Prim.sha256(abc) ==
      "\ba\78\16\bf\8f\01\cf\ea\41\41\40\de\5d\ae\22\23\b0\03\61\a3\96\17\7a\9c\b4\10\ff\61\f2\00\15\ad";

    // Hashing a 64 KiB input allocates just its small result, irrespective of
    // input length. This catches accidentally copying the borrowed input.
    let before = Prim.rts_total_allocation();
    let digest = Prim.sha256(large);
    assert digest.size() == 32;
    assert Prim.rts_total_allocation() < before + 256;
  };

  public func prepare() : async () {
    generation += 1;
    large := Prim.arrayToBlob(Prim.Array_tabulate<Nat8>(65_537,
      func(i) { Prim.natToNat8((i * 131 + 17) % 256) }));
    fixed := Prim.Array_tabulate<Fixed>(64, func(i) {
      {
        index = Prim.natToNat64(generation + i);
        payload = Prim.arrayToBlob([Prim.natToNat8(i)]);
      }
    });
    choices := Prim.Array_tabulate<Choice>(64, func(i) {
      if (i % 2 == 0) #ready else #seen(generation + i)
    });
    fixedOutput := Prim.to_candid_canonical(fixed);
    choicesOutput := Prim.to_candid_canonical(choices);
    retained := Prim.Array_tabulate<Group>(2, func(group) {
      { entries = Prim.Array_tabulate<Saved>(16, func(i) {
        let original : Entry = {
          index = generation * 100 + group * 16 + i;
          payload = Prim.arrayToBlob(Prim.Array_tabulate<Nat8>(55 + i,
            func(j) { Prim.natToNat8((i + j) % 256) }));
          memo = "group " # (debug_show group) # ": entry " # (debug_show i);
          metadata = if (i % 3 == 0) null else ?{
            tags = ["plugin", debug_show generation];
            state = if (i % 2 == 0) #ready else #seen(i);
          };
        };
        let encoded = Prim.to_candid_canonical(original);
        { original; outputs = [encoded, Prim.sha256(encoded)] }
      }) }
    });
    verify();
  };

  public func churn() : async () {
    reclaimedBefore := Prim.rts_reclaimed();
    // Nine allocations smaller than the 64 MiB GC partition roll allocation
    // into a new partition. The retained inputs/results stay in a sparse old
    // partition, making them candidates for evacuation rather than merely
    // triggering a collection that leaves their addresses unchanged.
    var i = 0;
    while (i < 9) {
      ignore Prim.Array_init<Nat>(1024 * 1024, i);
      i += 1;
    };
  };

  public func collectAndCheck() : async () {
    // --force-gc runs an increment at each suspension/message boundary.
    // Multiple increments allow marking, evacuation, and updating to finish.
    var i = 0;
    while (i < 4) {
      await async {};
      verify();
      i += 1;
    };
    assert Prim.rts_reclaimed() > reclaimedBefore + 32 * 1024 * 1024;
    Prim.debugPrint("compute plugins survived GC");
  };
};

//CALL ingress prepare "DIDL\x00\x00"
//CALL ingress churn "DIDL\x00\x00"
//CALL ingress collectAndCheck "DIDL\x00\x00"
//CALL ingress prepare "DIDL\x00\x00"
//CALL ingress churn "DIDL\x00\x00"
//CALL ingress collectAndCheck "DIDL\x00\x00"
