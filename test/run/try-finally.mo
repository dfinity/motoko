import { debugPrint; error } =  "mo:prim";

actor A {

    func t0() : async () {
        try { debugPrint "IN" }
        case { debugPrint "OUT" };
    };
/*  nested `try` won't work
    func t1() : async () {
        try {
            try {
                debugPrint "IN1";
                throw error "IN1";
            }
            case { debugPrint "OUT1" };
        }
        catch _ { debugPrint "CAUGHT1" }
    };
*/
    func t2() : async () {
        try {
            debugPrint "IN2";
            throw error "IN2";
        }
        else catch _ { debugPrint "CAUGHT2" }
        case { debugPrint "OUT2" };
    };

    func t3() : async () {
        try {
            debugPrint "IN3";
            return;
        }
        case { debugPrint "OUT3" };
    };

    // check that finally not running twice
    func t4() : async () {
        try {
            debugPrint "IN4";
        }
        case { debugPrint "OUT4" };
        return;
    };

    func t5() : async () {
        label out try {
            debugPrint "IN5";
            break out;
        }
        case { debugPrint "OUT5" };
    };

    public func go() : async () {
        await t0();
        //await t1();
        await t2();
        await t3();
        await t4();
        await t5();
    };
};

//SKIP comp
//SKIP run-low
//SKIP run-ir
//SKIP ic-ref-run

A.go();
