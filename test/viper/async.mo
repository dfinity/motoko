actor {

    var flag = false;

    public shared func claim() : async () {
        let a = async { flag := true };
        await a;
        if true {
            let b = async { flag := false };
        } else {
            let c = async { flag := false };
        };
        ()
    };

}
