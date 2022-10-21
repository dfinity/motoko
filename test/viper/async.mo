actor {

    var flag = false;

    public shared func claim() : async () {
        await async {
            assert:1:async not flag;
            flag := true
        };
        if true {
            await async {
                assert:1:async flag;
                flag := false
            };
        } else {
            await async {
                assert:1:async flag;
                flag := false
            };
        }
    };

}
