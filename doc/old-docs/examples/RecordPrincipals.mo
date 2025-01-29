import Principal "mo:base/Principal";
import OrderedSet "mo:base/OrderedSet";
import Error "mo:base/Error";

persistent actor {

    transient let principalSet = OrderedSet.Make<Principal>(Principal.compare);

    // Create set to record principals
    var principals : OrderedSet.Set<Principal> = principalSet.empty();

    // Check if principal is recorded
    public shared query(msg) func isRecorded() : async Bool {
        let caller = msg.caller;
        principalSet.contains(principals, caller);
    };

    // Record a new principal
    public shared(msg) func recordPrincipal() : async () {
        let caller = msg.caller;
        if (Principal.isAnonymous(caller)) {
            throw Error.reject("Anonymous principal not allowed");
        };

        principals := principalSet.put(principals, caller)
    };
};
