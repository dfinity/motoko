actor {

    var flag = false;

    public shared func claim() : async () {
        await async { flag := true };
        if true {
            await async { flag := false };
        } else {
            await async { flag := false };
        }
    };

}
