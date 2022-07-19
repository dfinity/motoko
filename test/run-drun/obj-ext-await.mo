actor a {
    public func go() : async () {
        let r = async ({ b = 42 });
        let s : { b : Nat; c : Char } = { c = 'C' in (await r) };
        let t : { b : Nat; cs : Text } = { cs = await async "Hi" in (await async s) };
        return;
    }
};
a.go(); //OR-CALL ingress go "DIDL\x00\x00"
