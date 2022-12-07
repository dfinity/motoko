actor {
    func bar() : async Nat {
        let 2 = await async 3 else { return await async 4 };
        5;
    };
    public func foo() {
        assert ((await bar()) == 4);
    };
}

//CALL ingress foo "DIDL\x00\x00"
