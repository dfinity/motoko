import Prim "mo:⛔";

actor A {

    let ic00 = actor "aaaaa-aa" : actor {
      canister_info : {
        canister_id : Principal;
        num_requested_changes : ?Nat64;
      } -> async Prim.Info;
    };

    public func dummy({ canister_id : Principal;
                 num_requested_changes : ?Nat64; }) : async Prim.Info
    {
        Prim.debugPrint "POST";
        return {
            total_num_changes = 0;
            recent_changes = [];
            module_hash = null;
            controllers = [];
        }
    };

    let pc = Prim.precompose2(func(p, n) {
        Prim.debugPrint "PRE";
        {
            canister_id = p;
            num_requested_changes = n
        } }, ic00.canister_info);

    public func go() : async () {
        let info0 = await ic00.canister_info { canister_id = Prim.principalOfActor A; num_requested_changes = ?4 };
        Prim.debugPrint(debug_show info0);
        let info = await pc(Prim.principalOfActor A, ?4);
        Prim.debugPrint(debug_show info)
    }
};

A.go(); //OR-CALL ingress go "DIDL\x00\x00"
