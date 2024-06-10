import { debugPrint; error; call_raw; principalOfActor } =  "mo:⛔";

actor A {
    func m() : async () {
    };

    public func raw() : async () {
    };

    func t0() : async () {
        try { debugPrint "IN"; await m(); assert false }
        finally { debugPrint "OUT" };
    };

    func t0r() : async () {
        let p = principalOfActor A;
        try { debugPrint "INr"; ignore await call_raw(p, "raw", to_candid()); assert false }
        finally { debugPrint "OUTr" };
    };

    func t0d() : async () {
        try { debugPrint "INd"; let fut = m(); await fut; debugPrint "AGAINd"; await fut; assert false }
        finally { debugPrint "OUTd" };
    };

/*  nested `try` won't work
    func t1() : async () {
        try {
            try {
                debugPrint "IN1";
                throw error "IN1";
            }
            finally { debugPrint "OUT1" };
        }
        catch _ { debugPrint "CAUGHT1" }
    };
*/
/*
    func t2() : async () {
        try {
            debugPrint "IN2";
            throw error "IN2";
        }
        else catch _ { debugPrint "CAUGHT2" }
        finally { debugPrint "OUT2" };
    };

    //TODO: func t2t() : async Int { ... }

    func t3() : async () {
        try {
            debugPrint "IN3";
            await m();
            return;
        }
        finally { debugPrint "OUT3" };
    };
/*
    // check that finally not running twice
    func t4() : async () {
        try {
            debugPrint "IN4";
        }
        finally { debugPrint "OUT4" };
        return;
    };
*/
    func t5() : async () {
        debugPrint "BEFORE5";
        label out try {
            debugPrint "IN5";
            await m();
            break out;
            debugPrint "DEAD5";
        }
        finally { debugPrint "OUT5" };
        debugPrint "AFTER5"
    };
*/
    func t6() : async () {
        debugPrint "BEFORE6";
        label out try {
            debugPrint "IN6";
            try {
                debugPrint "InnerIN6";
                await m();
                debugPrint "InnerLIVE6";
                break out;
                debugPrint "InnerDEAD6";
            } finally { debugPrint "InnerOUT6" };
            debugPrint "DEAD6";
        }
        finally { debugPrint "OUT6" };
        debugPrint "AFTER6"
    };

    // TODO: trap on happy/catch
    // TODO: trap after repeated `await`

    public func go() : async () {
        try /*ignore*/ await t0() catch _ {};
        try await t0r() catch _ {};
        try await t0d() catch _ {};
        //await t1();
        /*await t2();
        await t3();
        /*await t4();*/
        await t5();*/
        await t6();
    };
};

//SKIP ic-ref-run

//A.go(); //OR-CALL ingress go "DIDL\x00\x00"
