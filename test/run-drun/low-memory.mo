import Prim "mo:⛔";
import LowMemoryActor "low-memory/low-memory-actor";
import Cycles = "cycles/cycles";

actor Self {
  type canister_settings = {
    wasm_memory_threshold : ?Nat;
  };

  func setMemoryThreshold(a : actor {}, threshold : Nat) : async () {
    let ic00 = actor "aaaaa-aa" : actor {
      update_settings : shared {
        canister_id : Principal;
        settings : canister_settings;
      } -> async ();
    };
    let settings : canister_settings = {
      wasm_memory_threshold = ?threshold;
    };
    await ic00.update_settings({
      canister_id = Prim.principalOfActor(a);
      settings;
    });
  };

  let kb = 1024;
  let mb = 1024 * kb;
  let gb = 1024 * mb;
  let maxMemory = 4 * gb; // adjust when canister limits inspection is supported

  public shared func lowMemoryCallback() : async () {
    Prim.debugPrint("Low memory callback");
  };

  public shared func run() : async () {
    Cycles.add<system>(2_000_000_000_000);
    let lowMemoryActor1 = await LowMemoryActor.LowMemoryActor(lowMemoryCallback);
    // await lowMemoryActor1.allocateMemory();

    let threshold1 = maxMemory - (await lowMemoryActor1.memorySize()) - mb : Nat;
    await setMemoryThreshold(lowMemoryActor1, threshold1);
    await lowMemoryActor1.allocateMemory();

    // Not yet implemented on IC: Should retrigger when shrinking memory by reinstallation.
    // let lowMemoryActor2 = await (system LowMemoryActor.LowMemoryActor)(#reinstall lowMemoryActor1)(lowMemoryCallback);
    // await lowMemoryActor2.allocateMemory();
    // await lowMemoryActor2.allocateMemory();

    // Not yet implemented on IC: Should retrigger when lowering threshold.
    // let threshold2 = maxMemory - (await lowMemoryActor2.memorySize()) - mb : Nat;
    // await setMemoryThreshold(lowMemoryActor2, threshold2);
    // await lowMemoryActor2.allocateMemory();
  };
};

//SKIP run
//SKIP run-low
//SKIP run-ir
//SKIP ic-ref-run

//CALL ingress run "DIDL\x00\x00"
