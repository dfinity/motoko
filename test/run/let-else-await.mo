func bar() : async Nat { 4 };

func foo() : async Nat {
    let 2 = 3 else { return await bar() };
    5
};

assert ((await foo()) == 4);