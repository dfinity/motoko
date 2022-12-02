func foo() : async Nat {
    let 2 = 3 else { return 4 };
    5
};

assert (await foo()) == 4;