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

    func t2() : async () {
        try {
            debugPrint "IN2";
            throw error "IN2";
        }
        catch _ { debugPrint "CAUGHT2" }
        finally { debugPrint "OUT2" };
    };

    func t2i() : async Int {
        try {
            debugPrint "IN2i";
            await async ();
            throw error "IN2i";
        }
        catch _ { debugPrint "CAUGHT2i"; 42 }
        finally { debugPrint "OUT2i" };
    };
/*
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

    func t6t() : async () {
        debugPrint "BEFORE6t";
        label out try {
            debugPrint "IN6t";
            try {
                debugPrint "InnerIN6t";
                await m();
                debugPrint "InnerLIVE6t";
                assert false;
                debugPrint "InnerDEAD6t";
            } finally { debugPrint "InnerOUT6t" };
            debugPrint "DEAD6t";
        }
        finally { debugPrint "OUT6t" };
        debugPrint "AFTER6t"
    };

    func t6d() : async () {
        debugPrint "BEFORE6d";
        label out try {
            debugPrint "IN6d";
            try {
                debugPrint "InnerIN6d";
                let fut = m();
                await fut;
                debugPrint "InnerLIVE6d";
                await fut;
                debugPrint "InnerLIVESTILL6d";
                assert false;
                debugPrint "InnerDEAD6d";
            } finally { debugPrint "InnerOUT6d" };
            debugPrint "DEAD6d";
        }
        finally { debugPrint "OUT6d" };
        debugPrint "AFTER6d"
    };

    // TODO: trap on happy/catch
    // TODO: trap after repeated `await`

    public func go() : async () {
        try /*ignore*/ await t0() catch _ {};
        try await t0r() catch _ {};
        try await t0d() catch _ {};
        //await t1();
        await t2();
        ignore await t2i();
        /*await t3();
        /*await t4();*/
        await t5();*/
        await t6();
        try await t6t() catch _ {};
        try await t6d() catch _ {};
    };
};

//SKIP ic-ref-run

//A.go(); //OR-CALL ingress go "DIDL\x00\x00"
