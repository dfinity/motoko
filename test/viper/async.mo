// @verify

actor {

    var flag = false;

    public shared func claim() : async () {
            flag := true;
            flag := false;
        await async {
            assert:1:async not flag;
            flag := true;
            flag := false;
            flag := flag
        };
        if flag {
            await async {
                assert:1:async flag;
                flag := false
            }
        } else {
            await async {
                assert:1:async flag;
                flag := false
            }
        }
    }

}
