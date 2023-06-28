import Prim "mo:⛔";

actor A {

    func dummy({ canister_id : Principal;
                 num_requested_changes : ?Nat64; }) : async Prim.Info
    {
        return {
            total_num_changes = 0;
            recent_changes = [];
            module_hash = null;
            controllers = [];
        }
    };

    let pc = Prim.precompose2<Principal, ?Nat64, { canister_id : Principal;
                 num_requested_changes : ?Nat64; }>(func(p : Principal, n : ?Nat64) = {
                                                        canister_id = p;
                                                        num_requested_changes = n
                                                    }, dummy);

    public func go() : async () {
        let info = await pc(Prim.principalOfActor A, ?4);
        Prim.debugPrint(debug_show info)
    }
};

A.go(); //OR-CALL ingress go "DIDL\x00\x00"
